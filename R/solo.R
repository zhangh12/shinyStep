# ── solo.R ────────────────────────────────────────────────────────────────────
# Solo-mode module: the user edits one function's body + signature, supplies
# test values, and clicks Test. The package assembles `fn_name(args...)` and
# runs it with debug_targets = fn_name, so the editor pauses at the first
# body statement. Solo modules are never pause-points of a host-driven
# run_program() — they exist only to exercise a function in isolation.

#' UI for a solo-mode step debugger
#'
#' A solo module tests a single function in isolation. The editor holds the
#' function body only — do not type \code{fn_name <- function(...) \{ ... \}}
#' wrappers (they are stripped defensively if present). The function name and
#' argument list live above the editor as structured inputs, with a "test
#' value" column so each argument can be exercised.
#'
#' Pair with \code{\link{soloStepServer}()} using the same \code{id}.
#'
#' @param id Module namespace ID.
#' @param label Display label in the toolbar. Defaults to the id.
#' @param height Ace editor height as a CSS string.
#' @param theme Ace editor theme.
#' @param default_body Initial body text placed in the editor.
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
#' Registers the module with the runner, manages the editor, argument table,
#' and function-name input, and wires the built-in Test button.
#'
#' @param id Module namespace ID. Must match \code{soloStepUI()}.
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal(character(1))} shared across modules.
#' @param initial_fn_name Initial function name — static string or reactive.
#'   If blank, defaults to the module id.
#' @param initial_body Initial function body — static string or reactive.
#' @param initial_args Initial argument specs — list of
#'   \code{list(name, default, test_value)} entries, or a reactive returning
#'   such a list.
#' @param prelude Optional character string (or reactive returning one)
#'   prepended to the generated \code{main_code} on every Test click. Use it
#'   to load packages or define helpers the function depends on, e.g.
#'   \code{"library(dplyr)"}. Parsed as top-level R.
#'
#' @return A list of reactives/handles:
#'   \describe{
#'     \item{\code{save_clicked}, \code{back_clicked}}{Reactives firing on
#'       Save / Back clicks.}
#'     \item{\code{fn_name}}{Reactive — current function name.}
#'     \item{\code{get_fn_name}, \code{get_body}, \code{get_args}}{Functions
#'       returning the current persisted state.}
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
