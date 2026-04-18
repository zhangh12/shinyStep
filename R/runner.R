#' Create a shared debugger runner
#'
#' Call once inside your Shiny \code{server()} function. Pass the returned
#' object to every \code{stepServer()} call and to \code{run_program()}.
#'
#' @return A list with three fields:
#'   \describe{
#'     \item{state}{A \code{reactiveValues} holding all execution state.}
#'     \item{registry}{Environment mapping function names to their current code
#'       accessors (populated by \code{stepServer()}).}
#'     \item{pd}{Environment of \code{getParseData()} results keyed by function
#'       name (populated at run time, used for line-number tracking).}
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

      # ── Return-value fixup ───────────────────────────────────────────────
      # Main expression that triggered the current pause (e.g. x <- f(raw))
      pending_call_expr = NULL,
      # Last value evaluated inside the stepped function body
      fn_return_value   = NULL
    ),

    # Plain env: fn_name -> list(get_code = function())
    # Populated by stepServer(); read by run_program().
    registry = new.env(parent = emptyenv())
  )
}
