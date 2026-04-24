# ── embedded.R ────────────────────────────────────────────────────────────────
# Embedded-mode module: the function is called from inside a larger program
# that the host app assembles and launches via run_program(). This module
# contributes the function definition; when its Debug checkbox is ticked,
# run_program() installs a proxy that pauses at the first body expression
# every time the host program calls the function.

#' UI for an embedded-mode step debugger
#'
#' Renders the function editor with a \strong{Debug} checkbox. Ticking Debug
#' makes this function a pause point the next time the host app calls
#' \code{\link{run_program}()}. The editor holds the function body only — do
#' not type \code{fn_name <- function(...) \{ \}} wrappers; they are stripped
#' defensively if present. The function name and argument list live in
#' structured inputs above the editor.
#'
#' Pair with \code{\link{embeddedStepServer}()} using the same \code{id}.
#'
#' @param id Module namespace ID.
#' @param label Toolbar label. Defaults to \code{id}.
#' @param height Ace editor height as a CSS string (e.g. \code{"500px"}).
#' @param theme Ace editor theme name (passed to \pkg{shinyAce}).
#' @param default_body Initial body text pre-filled in the editor.
#' @return A \code{tagList} suitable for inclusion anywhere in a Shiny UI.
#' @export
embeddedStepUI <- function(id,
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
           show_debug      = TRUE,
           show_test       = FALSE,
           show_test_value = FALSE)
}

#' Server for an embedded-mode step debugger
#'
#' Registers the function with the shared runner and manages the editor,
#' argument table, and Debug checkbox. The host app calls
#' \code{\link{run_program}()} with its own \code{main_code}; this module
#' contributes a pause point automatically whenever Debug is ticked.
#'
#' @param id Module namespace ID. Must match \code{\link{embeddedStepUI}()}.
#' @param runner Runner object from \code{\link{make_runner}()}.
#' @param run_log A \code{reactiveVal(character(1))} shared across all modules
#'   and the host app.
#' @param initial_fn_name Initial function name — static string or reactive.
#' @param initial_body Initial function body — static string or reactive.
#' @param initial_args Initial argument specs — a list of
#'   \code{list(name, default)} entries, or a reactive returning such a list.
#'   (No \code{test_value} column — embedded functions are called from
#'   \code{main_code}, not via a Test button.)
#' @param lock_first_arg If \code{TRUE}, the first argument's name field is
#'   readonly and its delete button is hidden. Use this when the caller
#'   requires a reserved first parameter (e.g. a trial-engine contract that
#'   passes \code{trial} into every action).
#'
#' @return A named list:
#'   \describe{
#'     \item{\code{save_clicked}, \code{back_clicked}}{Reactives that fire on
#'       Save / Back clicks.}
#'     \item{\code{fn_name}}{Reactive returning the current function name.}
#'     \item{\code{enabled}}{Reactive — \code{TRUE} when the Debug checkbox
#'       is ticked.}
#'     \item{\code{get_fn_name()}, \code{get_body()}, \code{get_args()}}{
#'       Functions returning the current editor state (isolated reads).}
#'   }
#' @export
embeddedStepServer <- function(id, runner, run_log,
                               initial_fn_name = NULL,
                               initial_body    = NULL,
                               initial_args    = NULL,
                               lock_first_arg  = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    .step_server_core(
      input, output, session,
      runner          = runner,
      run_log         = run_log,
      type            = "embedded",
      initial_fn_name = initial_fn_name,
      initial_body    = initial_body,
      initial_args    = initial_args,
      show_test_value = FALSE,
      lock_first_arg  = lock_first_arg
    )
  })
}
