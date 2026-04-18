#' Server for a shinyStep debugger module
#'
#' Registers a function with the runner, manages the Ace editor state,
#' and wires all debug controls to the execution engine.
#'
#' @param id Module namespace ID. Must match the \code{id} passed to
#'   \code{stepUI()}.
#' @param fn_name The name under which this function will be installed in the
#'   execution environment (e.g. \code{"interim_fn"}).
#' @param runner Runner object from \code{make_runner()}.
#' @param run_log A \code{reactiveVal(character(1))} shared across all modules
#'   and passed to \code{run_program()}.
#' @param initial_code Optional initial function code as a character string
#'   \emph{or} a reactive that returns one. When \code{NULL} the editor starts
#'   with the \code{default_code} set in \code{stepUI()}.
#'
#' @return A list of signals the host app can observe:
#'   \describe{
#'     \item{\code{back_clicked}}{Reactive — fires when the user clicks Back.}
#'     \item{\code{fn_name}}{Character — the registered function name.}
#'     \item{\code{get_code}}{Function — returns the current editor text.}
#'     \item{\code{enabled}}{Reactive — \code{TRUE} when the Debug checkbox is ticked.}
#'   }
#' @export
stepServer <- function(id, fn_name, runner, run_log, initial_code = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rs <- runner$state

    # ── Code store ───────────────────────────────────────────────────────────
    # Resolve initial_code (static string, reactive, or NULL)
    start_code <- if (shiny::is.reactive(initial_code)) {
      shiny::isolate(initial_code())
    } else {
      initial_code
    }
    code_rv <- shiny::reactiveVal(start_code %||% "")

    # Register with runner so run_program() can read the current code
    runner$registry[[fn_name]] <- list(get_code = function() shiny::isolate(code_rv()))

    # Track whether editor has been modified during an active debug session
    modified_rv <- shiny::reactiveVal(FALSE)

    # ── Sync editor ↔ code_rv ────────────────────────────────────────────────
    shiny::observeEvent(input$code, {
      code_rv(input$code %||% "")
      # Flag as modified if we are mid-debug in this function
      if (isTRUE(rs$paused) && identical(rs$pause_owner, fn_name))
        modified_rv(TRUE)
    }, ignoreInit = FALSE)

    # If initial_code is a reactive, push updates into the editor
    if (shiny::is.reactive(initial_code)) {
      shiny::observeEvent(initial_code(), {
        val <- initial_code() %||% ""
        code_rv(val)
        shinyAce::updateAceEditor(session, "code", value = val)
      }, ignoreInit = FALSE)
    }

    # Reset modified flag when a new run starts
    shiny::observe({
      rs$running  # reactive dependency
      if (isTRUE(rs$running)) modified_rv(FALSE)
    })

    # Keep rs$debug_targets in sync with the Debug checkbox (live, even mid-run).
    # isolate() prevents rs$debug_targets from being a reactive dependency here,
    # which would cause all module observers to re-fire whenever any one updates it.
    shiny::observe({
      current <- shiny::isolate(rs$debug_targets)
      if (isTRUE(input$enabled)) {
        if (!fn_name %in% current) rs$debug_targets <- c(current, fn_name)
      } else {
        rs$debug_targets <- current[current != fn_name]
      }
    })

    # ── Status badge ─────────────────────────────────────────────────────────
    output$status_badge <- shiny::renderUI({
      if (isTRUE(rs$error)) {
        shiny::tags$span(class = "sStep-badge sStep-error", "Error")
      } else if (isTRUE(rs$ended)) {
        shiny::tags$span(class = "sStep-badge sStep-done", "Done")
      } else if (isTRUE(rs$paused) && identical(rs$pause_owner, fn_name)) {
        label <- if (isTRUE(modified_rv())) "Paused \u2014 modified" else
                   paste0("Paused \u2014 ", fn_name)
        cls   <- if (isTRUE(modified_rv())) "sStep-badge sStep-modified" else
                   "sStep-badge sStep-paused"
        shiny::tags$span(class = cls, label)
      } else if (isTRUE(rs$running)) {
        shiny::tags$span(class = "sStep-badge sStep-running", "Running\u2026")
      } else {
        shiny::tags$span(class = "sStep-badge sStep-ready", "Ready")
      }
    })

    # ── Green-arrow line highlight ───────────────────────────────────────────
    shiny::observe({
      paused_here <- isTRUE(rs$paused) && identical(rs$pause_owner, fn_name)
      if (!paused_here) {
        session$sendCustomMessage(ns("highlight_line"), list(line = NULL))
        return()
      }
      stack <- rs$step_stack
      n     <- length(stack)
      line  <- NULL
      if (n > 0L) {
        top <- stack[[n]]
        pc  <- top$pc
        lm <- top$line_map
        if (!is.null(lm) && pc >= 1L && pc <= length(lm) && !is.na(lm[[pc]])) {
          line <- as.integer(lm[[pc]])
        } else {
          # Fallback: srcref on the expression (rarely present after body extraction)
          expr <- current_expr(rs)
          if (!is.null(expr)) {
            sr <- attr(expr, "srcref")
            if (!is.null(sr)) line <- as.integer(sr[1L])
          }
        }
      }
      session$sendCustomMessage(ns("highlight_line"), list(line = line))
    })

    # ── Output log ───────────────────────────────────────────────────────────
    output$log_ui <- shiny::renderUI({
      txt <- run_log()
      if (!nzchar(txt))
        return(shiny::tags$span(style = "color:#555; font-style:italic;", "No output yet."))

      lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
      shiny::tagList(lapply(lines, function(line) {
        cls <- if      (startsWith(line, "[Error]"))  "sStep-log-line sStep-log-error"
               else if (startsWith(line, "[Paused]")) "sStep-log-line sStep-log-pause"
               else if (startsWith(line, "[Resume]")) "sStep-log-line sStep-log-resume"
               else if (startsWith(line, "[Done]"))   "sStep-log-line sStep-log-done"
               else if (startsWith(line, "[Info]"))   "sStep-log-line sStep-log-info"
               else if (startsWith(line, "> "))       "sStep-log-line sStep-log-console"
               else                                   "sStep-log-line sStep-log-out"
        shiny::div(class = cls, line)
      }))
    })

    # ── Step controls ────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_next, {
      shiny::req(isTRUE(rs$paused), identical(rs$pause_owner, fn_name))
      step_fn(runner, run_log)
    })

    shiny::observeEvent(input$btn_step_out, {
      shiny::req(isTRUE(rs$paused), identical(rs$pause_owner, fn_name))
      step_out_frame(runner, run_log)
    })

    shiny::observeEvent(input$btn_continue, {
      shiny::req(isTRUE(rs$paused), identical(rs$pause_owner, fn_name))
      continue_to_next_pause(runner, run_log)
    })

    shiny::observeEvent(input$btn_stop, {
      shiny::req(isTRUE(rs$running) || isTRUE(rs$paused))
      stop_runner(runner, run_log)
    })

    # ── Console evaluation ───────────────────────────────────────────────────
    .run_console <- function(code_txt) {
      shiny::req(isTRUE(rs$paused), identical(rs$pause_owner, fn_name))
      if (!nzchar(trimws(code_txt %||% ""))) return()

      expr <- tryCatch(
        parse(text = code_txt)[[1L]],
        error = function(e) call("stop", conditionMessage(e))
      )
      append_log(run_log, code_txt, type = "console")
      res <- eval_with_capture(expr, envir = rs$fun_frame, auto_print = TRUE)
      if (nzchar(res$output)) append_log(run_log, res$output)
      if (!is.null(res$error))
        append_log(run_log, conditionMessage(res$error), type = "error")
    }

    shiny::observeEvent(input$console_submit, { .run_console(input$console_submit) },
                        ignoreInit = TRUE)

    shiny::observeEvent(input$clear_log, { run_log("") }, ignoreInit = TRUE)

    # ── Return signals for the host app ─────────────────────────────────────
    list(
      back_clicked = shiny::reactive(input$back),
      fn_name      = fn_name,
      get_code     = function() code_rv(),
      enabled      = shiny::reactive(isTRUE(input$enabled))
    )
  })
}
