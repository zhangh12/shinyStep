# ── embedded.R ────────────────────────────────────────────────────────────────
# Embedded-mode module: the function is called from inside a larger program
# that the host app is responsible for assembling and launching via
# run_program(). The module contributes the function body + signature + fn
# name; when its Debug checkbox is ticked, run_program() pauses at the first
# line of the function each time the host program invokes it.

#' UI for an embedded-mode step debugger
#'
#' An embedded module describes a function that is called from inside a
#' larger program written and launched by the host app. The editor holds the
#' function body only — do not type \code{fn_name <- function(...) \{ ... \}}
#' wrappers (they are stripped defensively if present). The function name and
#' argument list live above the editor as structured inputs. A "Debug"
#' checkbox in the toolbar controls whether this function is a pause-point
#' during \code{\link{run_program}()}.
#'
#' Pair with \code{\link{embeddedStepServer}()} using the same \code{id}.
#'
#' @param id Module namespace ID.
#' @param label Display label in the toolbar. Defaults to the id.
#' @param height Ace editor height as a CSS string.
#' @param theme Ace editor theme.
#' @param default_body Initial body text placed in the editor.
#' @return A \code{tagList} suitable for inclusion anywhere in a Shiny UI.
#' @export
embeddedStepUI <- function(id,
                           label        = id,
                           height       = "500px",
                           theme        = "textmate",
                           default_body = "") {
  .step_ui(id,
           label           = label,
           height          = height,
           theme           = theme,
           default_body    = default_body,
           show_debug      = TRUE,
           show_test       = FALSE,
           show_test_value = FALSE)
}

#' Server for an embedded-mode step debugger
#'
#' Registers the module with the runner, manages the editor, argument table,
#' function-name input, and the Debug checkbox. The host app is responsible
#' for calling \code{\link{run_program}()} with its own \code{main_code}; this
#' module contributes a pause-point automatically when Debug is ticked.
#'
#' @param id Module namespace ID. Must match \code{embeddedStepUI()}.
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal(character(1))} shared across modules.
#' @param initial_fn_name Initial function name — static string or reactive.
#' @param initial_body Initial body text — static string or reactive.
#' @param initial_args Initial argument specs — list of
#'   \code{list(name, default)} entries, or a reactive returning such a list.
#'
#' @return A list of reactives/handles:
#'   \describe{
#'     \item{\code{save_clicked}, \code{back_clicked}}{Reactives firing on
#'       Save / Back clicks.}
#'     \item{\code{fn_name}}{Reactive — current function name.}
#'     \item{\code{enabled}}{Reactive — \code{TRUE} when Debug is ticked.}
#'     \item{\code{get_fn_name}, \code{get_body}, \code{get_args}}{Functions
#'       returning the current persisted state.}
#'   }
#' @export
embeddedStepServer <- function(id, runner, run_log,
                               initial_fn_name = NULL,
                               initial_body    = NULL,
                               initial_args    = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    .step_server_core(
      input, output, session,
      runner          = runner,
      run_log         = run_log,
      type            = "embedded",
      initial_fn_name = initial_fn_name,
      initial_body    = initial_body,
      initial_args    = initial_args,
      show_test_value = FALSE
    )
  })
}
