# ── solo.R ────────────────────────────────────────────────────────────────────
# Solo-mode module: the user edits a function body and signature, supplies
# test values for each argument, and clicks Test. The package assembles
# `fn_name(arg = value, ...)` and calls run_program() with debug_targets =
# fn_name, so execution pauses at the first body expression. Solo modules are
# never auto-paused during a host-driven run_program() call — they exist only
# to exercise a function in isolation via their own Test button.

#' UI for a solo-mode step debugger
#'
#' Renders the function editor with a \strong{Test} button. The editor holds
#' the function body only — do not type \code{fn_name <- function(...) \{ \}}
#' wrappers; they are stripped defensively if present. The function name and
#' argument list (including test values) live in structured inputs above the
#' editor.
#'
#' Pair with \code{\link{soloStepServer}()} using the same \code{id}.
#'
#' @param id Module namespace ID.
#' @param label Toolbar label. Defaults to \code{id}.
#' @param height Ace editor height as a CSS string (e.g. \code{"500px"}).
#' @param theme Ace editor theme name (passed to \pkg{shinyAce}).
#' @param default_body Initial body text pre-filled in the editor.
#' @return A \code{tagList} suitable for inclusion anywhere in a Shiny UI.
#' @export
soloStepUI <- function(id,
                       label        = id,
                       height       = "500px",
                       theme        = "textmate",
                       default_body = "") {
  .step_ui(id,
           label           = label,
           height          = height,
           theme           = theme,
           default_body    = default_body,
           show_debug      = FALSE,
           show_test       = TRUE,
           show_test_value = TRUE)
}

#' Server for a solo-mode step debugger
#'
#' Registers the function with the shared runner, manages the editor and
#' argument table, and wires the \strong{Test} button. Clicking Test assembles
#' \code{fn_name(arg = test_value, ...)} and calls \code{\link{run_program}()}
#' with \code{debug_targets = fn_name}, pausing at the first body expression.
#'
#' @param id Module namespace ID. Must match \code{\link{soloStepUI}()}.
#' @param runner Runner object from \code{\link{make_runner}()}.
#' @param run_log A \code{reactiveVal(character(1))} shared across all modules
#'   and the host app.
#' @param initial_fn_name Initial function name — static string or reactive.
#'   Leave \code{NULL} or \code{""} to start unnamed.
#' @param initial_body Initial function body — static string or reactive.
#' @param initial_args Initial argument specs — a list of
#'   \code{list(name, default, test_value)} entries, or a reactive returning
#'   such a list. \code{test_value} is used by the Test button;
#'   \code{default} is the function signature default.
#' @param prelude Optional character string or reactive prepended to the
#'   generated call on every Test click. Use it to load packages or define
#'   helpers the function needs, e.g. \code{"library(dplyr)"}.
#'
#' @return A named list:
#'   \describe{
#'     \item{\code{save_clicked}, \code{back_clicked}}{Reactives that fire on
#'       Save / Back clicks. Wire these in the host app to persist or discard
#'       the editor state.}
#'     \item{\code{fn_name}}{Reactive returning the current function name.}
#'     \item{\code{get_fn_name()}, \code{get_body()}, \code{get_args()}}{
#'       Functions returning the current editor state (isolated reads).}
#'   }
#' @export
soloStepServer <- function(id, runner, run_log,
                           initial_fn_name = NULL,
                           initial_body    = NULL,
                           initial_args    = NULL,
                           prelude         = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    handles <- .step_server_core(
      input, output, session,
      runner          = runner,
      run_log         = run_log,
      type            = "solo",
      initial_fn_name = initial_fn_name,
      initial_body    = initial_body,
      initial_args    = initial_args,
      show_test_value = TRUE
    )

    # ── Test button: run fn_name(<test values>) in isolation ──────────────
    shiny::observeEvent(input$btn_test, {
      fn_name <- handles$get_fn_name()
      if (!is_valid_r_name(fn_name)) {
        append_log(run_log,
                   paste0("Invalid function name: '", fn_name, "'"),
                   type = "error")
        return()
      }
      body <- handles$get_body()
      if (!nzchar(trimws(body))) {
        append_log(run_log, "Body is empty — nothing to run.", type = "error")
        return()
      }
      args      <- handles$get_args()
      call_args <- .format_call_from_testvals(args)
      main_code <- if (nzchar(call_args)) sprintf("%s(%s)", fn_name, call_args)
                   else                   sprintf("%s()", fn_name)

      prelude_txt <- if (shiny::is.reactive(prelude)) prelude() else prelude
      prelude_txt <- prelude_txt %||% ""
      if (nzchar(trimws(prelude_txt)))
        main_code <- paste0(prelude_txt, "\n", main_code)

      run_program(
        runner        = runner,
        main_code     = main_code,
        debug_targets = fn_name,   # always pause at entry in solo Test
        run_log       = run_log
      )
    }, ignoreInit = TRUE)

    handles
  })
}
