#' Create a shared debugger runner
#'
#' Call once inside your Shiny \code{server()} function. Pass the returned
#' object to every \code{\link{soloStepServer}()} /
#' \code{\link{embeddedStepServer}()} call and to \code{\link{run_program}()}.
#'
#' @return A list with three named fields:
#'   \describe{
#'     \item{state}{A \code{reactiveValues} holding all execution state
#'       (running, paused, step stack, environments, etc.).}
#'     \item{registry}{Environment mapping module ids to their accessor
#'       functions. Populated automatically by each step-server module.}
#'     \item{pd}{Environment of \code{getParseData()} results keyed by module
#'       id. Populated at run time and used for editor line-number tracking.}
#'   }
#' @export
make_runner <- function() {
  list(
    # Per-function getParseData results — populated by run_program(), used by
    # step_fn() to look up body-statement line numbers for nested frames.
    # Must be an environment (not a list) so mutations inside .init_runner
    # are visible to step_fn() — lists are copied on assignment in R.
    pd = new.env(parent = emptyenv()),

    state = shiny::reactiveValues(
      # ── Execution flags ──────────────────────────────────────────────────
      running      = FALSE,
      paused       = FALSE,
      pause_owner  = NULL,   # fn_name currently paused, or NULL
      ended        = FALSE,
      error        = FALSE,

      # ── Environments ────────────────────────────────────────────────────
      env          = NULL,   # top-level execution env (parent = .GlobalEnv)
      fun_frame    = NULL,   # env for the paused function's local variables

      # ── Step stack ──────────────────────────────────────────────────────
      # Each frame: list(type, exprs, pc, [...type-specific fields])
      #   type = "fn"     : function body
      #   type = "for"    : for-loop body (+ var_name, values, iter)
      #   type = "while"  : while-loop body (+ cond_expr)
      #   type = "repeat" : repeat-loop body
      #   type = "if"     : if/else branch body
      step_stack   = list(),

      # ── Main program state ───────────────────────────────────────────────
      main_exprs   = list(),
      main_pc      = 1L,

      # ── Debug targets ────────────────────────────────────────────────────
      debug_targets = character(0),
      debug_skip    = character(0),

      # ── Return-value fixup ───────────────────────────────────────────────
      # Main expression that triggered the current pause (e.g. x <- f(raw))
      pending_call_expr = NULL,
      # Last value evaluated inside the stepped function body
      fn_return_value   = NULL
    ),

    # Plain env: module_id -> list(type, fn_name, get_body, get_args, enabled)
    # Populated by soloStepServer() / embeddedStepServer(); read by run_program().
    registry = new.env(parent = emptyenv())
  )
}
