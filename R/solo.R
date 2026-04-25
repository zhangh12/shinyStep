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
#' @param default_fn_name Initial function name pre-filled in the name input.
#'   Use this when re-mounting the editor for an already-named function so the
#'   field is populated on first render (the reactive \code{initial_fn_name}
#'   alone cannot seed the DOM that is created later).
#' @return A \code{tagList} suitable for inclusion anywhere in a Shiny UI.
#' @export
soloStepUI <- function(id,
                       label           = id,
                       height          = "500px",
                       theme           = "textmate",
                       default_body    = "",
                       default_fn_name = "") {
  .step_ui(id,
           label           = label,
           height          = height,
           theme           = theme,
           default_body    = default_body,
           default_fn_name = default_fn_name,
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
#' @param reserved_args Optional list of reserved-argument specs. Each entry
#'   pins one row at the top of the argument table: the name field is readonly,
#'   the delete button is hidden, and the user cannot rename, remove, or
#'   reorder the row. Entries may be a bare character name (\code{"n"}) or a
#'   list with fields:
#'   \describe{
#'     \item{\code{name}}{The reserved argument name (required).}
#'     \item{\code{allow_default}}{If \code{FALSE}, the default-value field is
#'       hidden for this row. Defaults to \code{TRUE}.}
#'     \item{\code{allow_test_value}}{If \code{FALSE}, the test-value field is
#'       hidden for this row. Defaults to \code{TRUE}.}
#'   }
#'   Most callers only need to pin names (e.g. \code{list("n")} for a
#'   simulator that always passes \code{n}); disallowing both value fields is
#'   unusual but useful when the host supplies the argument at call time (e.g.
#'   \code{list(list(name = "trial", allow_default = FALSE,
#'   allow_test_value = FALSE))}).
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
                           prelude         = NULL,
                           reserved_args   = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    handles <- .step_server_core(
      input, output, session,
      runner          = runner,
      run_log         = run_log,
      type            = "solo",
      initial_fn_name = initial_fn_name,
      initial_body    = initial_body,
      initial_args    = initial_args,
      show_test_value = TRUE,
      reserved_args   = reserved_args
    )

    # ── Merged Test/Next button: run fn_name(<test values>) in isolation ─
    # btn_next doubles as the Test entry-point in solo mode (see
    # .editor_and_debug_pane). When the runner is already paused inside this
    # function, editor_core's own btn_next observer handles stepping; the
    # guard below makes this observer a no-op in that case so we don't also
    # kick off a fresh run on top of the paused one.
    rs <- runner$state
    shiny::observeEvent(input$btn_next, {
      fn_name <- handles$get_fn_name()
      paused_here <- isTRUE(rs$paused) && identical(rs$pause_owner, fn_name)
      if (paused_here) return()          # step_fn handled in editor_core.R
      if (isTRUE(rs$running)) return()   # already mid-run; ignore
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
