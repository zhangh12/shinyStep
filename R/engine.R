# ── Return-value fixup ───────────────────────────────────────────────────────

# After a debugged function completes, write the correct return value back into
# rs$env for any pending assignment (x <- f() or x = f()).
.fixup_pending_assign <- function(rs) {
  pexpr <- rs$pending_call_expr
  if (is.null(pexpr) || !is.call(pexpr)) return(invisible())
  head <- pexpr[[1L]]
  if ((identical(head, as.name("<-")) || identical(head, as.name("="))) &&
      is.name(pexpr[[2L]]) && !is.null(rs$env)) {
    assign(as.character(pexpr[[2L]]), rs$fn_return_value, envir = rs$env)
  }
  rs$pending_call_expr <- NULL
}

# ── Stack helpers ────────────────────────────────────────────────────────────

# Return the expression that will execute on the next step, or NULL.
# NOTE: also returns NULL when the expression IS the NULL literal — use
# has_current_expr() to distinguish "nothing left" from "expression is NULL".
current_expr <- function(rs) {
  n <- length(rs$step_stack)
  if (n == 0L) return(NULL)
  top <- rs$step_stack[[n]]
  if (top$pc > length(top$exprs)) return(NULL)
  top$exprs[[top$pc]]
}

# TRUE iff the top frame has an expression at the current pc (even if NULL).
has_current_expr <- function(rs) {
  n <- length(rs$step_stack)
  if (n == 0L) return(FALSE)
  top <- rs$step_stack[[n]]
  top$pc <= length(top$exprs)
}

# Advance the top frame's pc; handle frame exhaustion and popping.
# When the fn frame empties: sets rs$paused = FALSE (caller resumes main).
.advance_stack <- function(rs, run_log) {
  n <- length(rs$step_stack)
  if (n == 0L) return(invisible())

  stack     <- rs$step_stack
  top       <- stack[[n]]
  top$pc    <- top$pc + 1L

  if (top$pc <= length(top$exprs)) {
    stack[[n]]    <- top
    rs$step_stack <- stack
    return(invisible())
  }

  # ── Frame exhausted ──────────────────────────────────────────────────────
  if (top$type == "for") {
    nxt <- top$iter + 1L
    if (nxt <= length(top$values)) {
      assign(top$var_name, top$values[[nxt]], envir = rs$fun_frame)
      top$pc   <- 1L
      top$iter <- nxt
      stack[[n]]    <- top
      rs$step_stack <- stack
    } else {
      rs$step_stack <- stack[-n]
      .advance_stack(rs, run_log)
    }

  } else if (top$type == "while") {
    cond <- tryCatch(
      isTRUE(eval(top$cond_expr, envir = rs$fun_frame)),
      error = function(e) FALSE
    )
    if (cond) {
      top$pc        <- 1L
      stack[[n]]    <- top
      rs$step_stack <- stack
    } else {
      rs$step_stack <- stack[-n]
      .advance_stack(rs, run_log)
    }

  } else if (top$type == "repeat") {
    # repeat loops restart unless break was used (break pops the frame)
    top$pc        <- 1L
    stack[[n]]    <- top
    rs$step_stack <- stack

  } else {
    # "fn" or "if" frame done — pop
    rs$step_stack <- stack[-n]
    if (length(rs$step_stack) == 0L) {
      .fixup_pending_assign(rs)
      rs$debug_skip  <- c(rs$debug_skip, rs$pause_owner)
      rs$paused      <- FALSE
      rs$pause_owner <- NULL
      append_log(run_log, "Function complete.", type = "resume")
    } else {
      .advance_stack(rs, run_log)
    }
  }
}

# Pop frames until (and including) the nearest loop frame — handles break.
.handle_break <- function(rs, run_log) {
  loop_types <- c("for", "while", "repeat")
  repeat {
    n <- length(rs$step_stack)
    if (n == 0L) break
    top <- rs$step_stack[[n]]
    rs$step_stack <- rs$step_stack[-n]
    if (top$type %in% loop_types) break
  }
  if (length(rs$step_stack) == 0L) {
    rs$debug_skip  <- c(rs$debug_skip, rs$pause_owner)
    rs$paused      <- FALSE
    rs$pause_owner <- NULL
    append_log(run_log, "Function complete (break).", type = "resume")
  } else {
    .advance_stack(rs, run_log)
  }
}

# For "next": finish current loop body iteration, start the next one.
.handle_next_kw <- function(rs, run_log) {
  loop_types <- c("for", "while", "repeat")
  n <- length(rs$step_stack)
  # Pop non-loop frames until we reach the enclosing loop
  while (n > 0L && !(rs$step_stack[[n]]$type %in% loop_types)) {
    rs$step_stack <- rs$step_stack[-n]
    n <- length(rs$step_stack)
  }
  if (n == 0L) {
    rs$debug_skip  <- c(rs$debug_skip, rs$pause_owner)
    rs$paused      <- FALSE
    rs$pause_owner <- NULL
    append_log(run_log, "Function complete (next).", type = "resume")
    return(invisible())
  }
  # Trigger iteration advance by setting pc past the end of the loop body
  top       <- rs$step_stack[[n]]
  top$pc    <- length(top$exprs) + 1L
  stack     <- rs$step_stack
  stack[[n]] <- top
  rs$step_stack <- stack
  .advance_stack(rs, run_log)
}

# After any step: if function completed, resume main program.
.maybe_resume_main <- function(runner, run_log) {
  rs <- runner$state
  if (!isTRUE(rs$paused) && isTRUE(rs$running) && !isTRUE(rs$error))
    run_main_until_pause_or_end(runner, run_log)
}

# ── Atomic single-expression step (private) ──────────────────────────────────

.step_one <- function(runner, run_log) {
  rs   <- runner$state
  if (!has_current_expr(rs)) return(invisible())
  expr <- current_expr(rs)

  # ── Special flow-control keywords ────────────────────────────────────────
  if (is_return_expr(expr)) {
    rs$fn_return_value <- if (length(expr) > 1L)
      tryCatch(eval(expr[[2L]], envir = rs$fun_frame), error = function(e) NULL)
    else invisible(NULL)
    .fixup_pending_assign(rs)
    rs$debug_skip  <- c(rs$debug_skip, rs$pause_owner)
    rs$step_stack  <- list()
    rs$paused      <- FALSE
    rs$pause_owner <- NULL
    append_log(run_log, "Function returned.", type = "resume")
    return(invisible())
  }
  if (is_break_expr(expr)) { .handle_break(rs, run_log);    return(invisible()) }
  if (is_next_expr(expr))  { .handle_next_kw(rs, run_log);  return(invisible()) }

  # ── Evaluate and capture output ──────────────────────────────────────────
  res <- eval_with_capture(expr, rs$fun_frame)
  if (nzchar(res$output)) append_log(run_log, res$output)
  if (!is.null(res$error)) {
    append_log(run_log, conditionMessage(res$error), type = "error")
    rs$error <- TRUE; rs$paused <- FALSE; rs$running <- FALSE
    return(invisible())
  }

  rs$fn_return_value <- res$value
  .advance_stack(rs, run_log)
}

# ── Public step controls ─────────────────────────────────────────────────────

#' Execute the next expression, auto-expanding compound blocks (Next button)
#'
#' For simple expressions, evaluates atomically. For compound expressions
#' (\code{for}/\code{while}/\code{repeat}/\code{if}), pushes their body onto
#' the step stack so sub-expressions can be stepped individually. Matches
#' RStudio debugger behaviour — no separate Step Into needed.
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal} for log output.
#' @export
step_fn <- function(runner, run_log) {
  rs <- runner$state
  if (!isTRUE(rs$paused)) return(invisible())

  if (!has_current_expr(rs)) return(invisible())
  expr <- current_expr(rs)

  if (!is_compound(expr)) {
    .step_one(runner, run_log)
    .maybe_resume_main(runner, run_log)
    return(invisible())
  }

  # Line of the compound expression in the editor (for body line_map lookup)
  .cur_line <- local({
    stk <- rs$step_stack; n <- length(stk)
    if (n == 0L) return(NULL)
    lm <- stk[[n]]$line_map; pc <- stk[[n]]$pc
    if (!is.null(lm) && pc >= 1L && pc <= length(lm) && !is.na(lm[[pc]]))
      as.integer(lm[[pc]]) else NULL
  })
  .pd <- runner$pd[[rs$pause_owner]]

  # Build line_map for a body block: pd-lookup first, srcref fallback.
  .bm <- function(exprs, branch_idx = 1L) {
    lm <- .compound_body_lines_from_pd(.pd, .cur_line, branch_idx)
    if (is.null(lm)) lm <- .exprs_line_map(exprs)
    lm
  }

  if (is_for_loop(expr)) {
    seq_vals <- tryCatch(
      as.list(eval(expr[[3L]], envir = rs$fun_frame)),
      error = function(e) {
        append_log(run_log, paste0("for sequence: ", conditionMessage(e)), type = "error")
        rs$error <- TRUE; rs$paused <- FALSE; rs$running <- FALSE
        NULL
      }
    )
    if (is.null(seq_vals) || isTRUE(rs$error)) return(invisible())

    var_name   <- as.character(expr[[2L]])
    body_exprs <- split_body_from_expr(expr[[4L]])

    if (length(seq_vals) == 0L) {
      .advance_stack(rs, run_log)
      .maybe_resume_main(runner, run_log)
      return(invisible())
    }
    assign(var_name, seq_vals[[1L]], envir = rs$fun_frame)
    rs$step_stack <- c(rs$step_stack, list(
      list(type = "for", exprs = body_exprs, pc = 1L,
           var_name = var_name, values = seq_vals, iter = 1L,
           line_map = .bm(body_exprs))
    ))

  } else if (is_while_loop(expr)) {
    cond <- tryCatch(
      isTRUE(eval(expr[[2L]], envir = rs$fun_frame)),
      error = function(e) {
        append_log(run_log, paste0("while condition: ", conditionMessage(e)), type = "error")
        rs$error <- TRUE; rs$paused <- FALSE; rs$running <- FALSE
        FALSE
      }
    )
    if (isTRUE(rs$error)) return(invisible())
    if (!cond) {
      .advance_stack(rs, run_log)
      .maybe_resume_main(runner, run_log)
      return(invisible())
    }
    body_exprs <- split_body_from_expr(expr[[3L]])
    rs$step_stack <- c(rs$step_stack, list(
      list(type = "while", exprs = body_exprs, pc = 1L,
           cond_expr = expr[[2L]], line_map = .bm(body_exprs))
    ))

  } else if (is_repeat_loop(expr)) {
    body_exprs <- split_body_from_expr(expr[[2L]])
    rs$step_stack <- c(rs$step_stack, list(
      list(type = "repeat", exprs = body_exprs, pc = 1L,
           line_map = .bm(body_exprs))
    ))

  } else if (is_if_expr(expr)) {
    cond <- tryCatch(
      isTRUE(eval(expr[[2L]], envir = rs$fun_frame)),
      error = function(e) {
        append_log(run_log, paste0("if condition: ", conditionMessage(e)), type = "error")
        rs$error <- TRUE; rs$paused <- FALSE; rs$running <- FALSE
        NULL
      }
    )
    if (isTRUE(rs$error)) return(invisible())

    branch_idx <- if (isTRUE(cond)) 1L else 2L
    branch     <- if (isTRUE(cond)) expr[[3L]] else if (length(expr) > 3L) expr[[4L]] else NULL

    if (is.null(branch)) {
      .advance_stack(rs, run_log)
      .maybe_resume_main(runner, run_log)
    } else {
      branch_exprs <- split_body_from_expr(branch)
      rs$step_stack <- c(rs$step_stack, list(
        list(type = "if", exprs = branch_exprs, pc = 1L,
             line_map = .bm(branch_exprs, branch_idx))
      ))
    }
  }
}

#' Step out of the current loop or if/else block (Step Out button)
#'
#' Pops the innermost non-function frame, landing at the next statement in the
#' enclosing scope. For example, when paused inside an inner \code{for} loop,
#' clicking Step Out exits that loop and resumes at the next statement of the
#' outer loop body. No-op when only the function frame remains on the stack.
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal} for log output.
#' @export
step_out_frame <- function(runner, run_log) {
  rs <- runner$state
  if (!isTRUE(rs$paused)) return(invisible())

  n <- length(rs$step_stack)
  if (n == 0L) return(invisible())

  top <- rs$step_stack[[n]]
  if (top$type == "fn") return(invisible())  # no-op at function level

  # For every non-fn frame (for/while/repeat/if): drain remaining expressions
  # atomically until this frame has naturally exited.  .step_one is atomic so
  # nested compound expressions (inner loops, if blocks) run to completion.
  while (!isTRUE(rs$error) && length(rs$step_stack) >= n) {
    .step_one(runner, run_log)
  }
  .maybe_resume_main(runner, run_log)
}

#' Continue execution to the next registered pause point (Continue button)
#'
#' Drains all remaining expressions in the currently paused function, then
#' resumes the main program until the next registered function is entered or
#' the program finishes.
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal} for log output.
#' @export
continue_to_next_pause <- function(runner, run_log) {
  rs <- runner$state
  if (!isTRUE(rs$running)) return(invisible())

  # Drain remaining steps atomically
  while (isTRUE(rs$paused) && !isTRUE(rs$error) && length(rs$step_stack) > 0L) {
    .step_one(runner, run_log)
  }

  # Resume main (will pause again at the next registered function, or finish)
  if (!isTRUE(rs$error))
    run_main_until_pause_or_end(runner, run_log)
}

#' Stop execution and reset the runner (Stop button)
#' @param runner Runner from \code{make_runner()}.
#' @param run_log A \code{reactiveVal} for log output.
#' @export
stop_runner <- function(runner, run_log) {
  rs <- runner$state
  if (isTRUE(rs$running) || isTRUE(rs$paused))
    append_log(run_log, "Stopped by user.", type = "info")
  rs$running     <- FALSE
  rs$paused      <- FALSE
  rs$pause_owner <- NULL
  rs$ended       <- TRUE
  rs$step_stack  <- list()
}

# ── Main program execution ───────────────────────────────────────────────────

# Run main expressions sequentially until a pause, error, or completion.
run_main_until_pause_or_end <- function(runner, run_log) {
  rs <- runner$state
  if (!isTRUE(rs$running) || isTRUE(rs$ended) || isTRUE(rs$error)) return(invisible())

  exprs <- rs$main_exprs
  if (isTRUE(attr(exprs, "parse_error"))) {
    append_log(run_log, paste0("Main parse error: ", attr(exprs, "parse_msg")), type = "error")
    rs$error <- TRUE; rs$running <- FALSE
    return(invisible())
  }

  while (rs$main_pc <= length(exprs)) {
    if (isTRUE(rs$paused) || isTRUE(rs$error)) break
    res <- eval_with_capture(exprs[[rs$main_pc]], rs$env)
    if (nzchar(res$output)) append_log(run_log, res$output)
    # Check pause BEFORE error: the proxy sets rs$paused then throws a condition
    # to abort any enclosing synchronous call (e.g. controller$run()).
    # That condition may be re-wrapped as a generic error by third-party code,
    # so checking rs$paused first ensures it is treated as a pause, not an error.
    if (isTRUE(rs$paused)) {
      rs$fn_return_value   <- NULL
      rs$pending_call_expr <- exprs[[rs$main_pc]]
      # Direct call: the main expression IS a call to the paused function
      # (e.g. `action1(x)` or `result <- action1(x)`).  Advance past it;
      # .fixup_pending_assign handles any assignment on the left-hand side.
      #
      # Nested call: the paused function was called from inside another
      # expression (e.g. `controller$run()` which internally calls action1).
      # Keep main_pc so the outer expression is re-executed after debugging —
      # the just-debugged function is in debug_skip and passes through silently,
      # allowing subsequent pause points in the same outer call to fire.
      pexpr     <- exprs[[rs$main_pc]]
      call_expr <- if (is.call(pexpr) &&
                       (identical(pexpr[[1L]], as.name("<-")) ||
                        identical(pexpr[[1L]], as.name("=")))) pexpr[[3L]] else pexpr
      if (is.call(call_expr) && identical(call_expr[[1L]], as.name(rs$pause_owner))) {
        # Direct call: main expression IS the paused function — advance past it.
        # .fixup_pending_assign handles any assignment on the left-hand side.
        rs$main_pc <- rs$main_pc + 1L
      } else {
        # Nested call: the paused function was invoked from inside another
        # expression (e.g. controller$run() called action1 internally).
        # Reset to pc = 1 so all setup code reruns, recreating fresh R6 objects
        # before the outer call is reached again.  The just-debugged function is
        # in debug_skip and passes through silently on the replay, allowing the
        # next pause point (e.g. action2) to fire.
        rs$main_pc <- 1L
      }
      break
    }
    if (!is.null(res$error)) {
      append_log(run_log, paste0("Main: ", conditionMessage(res$error)), type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    rs$main_pc <- rs$main_pc + 1L
  }

  if (!isTRUE(rs$paused) && !isTRUE(rs$error) && rs$main_pc > length(exprs)) {
    rs$ended <- TRUE; rs$running <- FALSE
    append_log(run_log, "Program finished.", type = "done")
  }
}

# ── Runner initialisation ────────────────────────────────────────────────────

.init_runner <- function(runner, main_code, debug_targets, run_log) {
  rs  <- runner$state
  reg <- runner$registry

  rs$running            <- TRUE
  rs$paused             <- FALSE
  rs$pause_owner        <- NULL
  rs$ended              <- FALSE
  rs$error              <- FALSE
  rs$debug_targets      <- debug_targets
  rs$debug_skip         <- character(0)
  rs$step_stack         <- list()
  rs$env                <- new.env(parent = .GlobalEnv)
  rs$fun_frame          <- NULL
  rs$pending_call_expr  <- NULL
  rs$fn_return_value    <- NULL

  # Reset parse-data cache for this run (clear the env in-place — can't
  # replace runner$pd with a new list because runner is pass-by-value)
  rm(list = ls(runner$pd, all.names = TRUE), envir = runner$pd)

  # Assemble, parse, and compile each registered module.
  # Registry entry shape (both solo and embedded):
  #   list(type, fn_name, get_body, get_args, enabled)
  # Modules with a blank body are silently skipped — the proxy is not
  # installed. If main_code references such a function, the user sees a
  # standard "could not find function" error, which is actionable.
  user_fns  <- list()
  line_maps <- list()
  seen_names <- character(0)

  for (mod_id in ls(reg)) {
    entry <- reg[[mod_id]]
    fn_name <- tryCatch(entry$fn_name(), error = function(e) NULL) %||% mod_id
    body    <- tryCatch(entry$get_body(), error = function(e) "") %||% ""
    args    <- tryCatch(entry$get_args(), error = function(e) list()) %||% list()

    if (!nzchar(trimws(body))) next  # blank editor — skip silently

    if (!is_valid_r_name(fn_name)) {
      append_log(run_log,
        paste0("Invalid function name '", fn_name, "' (module '", mod_id, "')."),
        type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    if (fn_name %in% seen_names) {
      append_log(run_log,
        paste0("Duplicate function name '", fn_name, "' — each module must have a unique name."),
        type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    seen_names <- c(seen_names, fn_name)

    code_txt <- .assemble_fn_code(fn_name, body, args)
    parsed   <- tryCatch(parse(text = code_txt, keep.source = TRUE), error = function(e) e)
    if (inherits(parsed, "error")) {
      append_log(run_log,
        paste0("Failed to parse '", fn_name, "': ", conditionMessage(parsed)),
        type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    runner$pd[[fn_name]] <- tryCatch(
      utils::getParseData(parsed, includeText = FALSE),
      error = function(e) NULL
    )
    fn_obj <- tryCatch(eval(parsed, envir = rs$env), error = function(e) e)
    if (inherits(fn_obj, "error")) {
      append_log(run_log,
        paste0("Failed to evaluate '", fn_name, "': ", conditionMessage(fn_obj)),
        type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    if (!is.function(fn_obj)) {
      append_log(run_log, paste0("'", fn_name, "' did not evaluate to a function."),
                 type = "error")
      rs$error <- TRUE; rs$running <- FALSE
      return(invisible())
    }
    user_fns[[fn_name]]  <- fn_obj
    line_maps[[fn_name]] <- parse_body_lines(code_txt)
  }

  # Install proxy closures — intercept calls from main code
  for (fn_name in names(user_fns)) {
    local({
      .name     <- fn_name
      .user_fn  <- user_fns[[fn_name]]
      .line_map <- line_maps[[fn_name]]

      # Build a proxy whose formals mirror the user function's formals (so that
      # any external signature validation — e.g. TrialSimulator checking that
      # the first arg is named "trial" — sees the correct signature).
      # We always append "..." if the user function doesn't have it, so the
      # body can safely collect extra arguments.
      .proxy <- function() {
        # Collect all named args from this frame + any extra "..." args
        .self_fmls <- names(formals(sys.function()))
        .named     <- .self_fmls[.self_fmls != "..."]
        .env       <- environment()
        call_args  <- lapply(.named, function(nm) get(nm, envir = .env, inherits = FALSE))
        if ("..." %in% .self_fmls)
          call_args <- c(call_args, eval(quote(list(...)), envir = .env))

        if (.name %in% rs$debug_targets && !(.name %in% rs$debug_skip)) {
          # Set up paused frame with actual call arguments + defaults
          rs$fun_frame <- new.env(parent = rs$env)
          fmls      <- formals(.user_fn)
          fml_names <- names(fmls)
          for (i in seq_along(fml_names)) {
            nm <- fml_names[[i]]
            if (nm == "...") next
            if (i <= length(call_args)) {
              assign(nm, call_args[[i]], envir = rs$fun_frame)
            } else if (!identical(fmls[[nm]], quote(expr = ))) {
              # Formal has a default — evaluate it in the fun_frame so later
              # defaults can reference earlier args (e.g. n = length(x))
              tryCatch(
                assign(nm, eval(fmls[[nm]], envir = rs$fun_frame),
                       envir = rs$fun_frame),
                error = function(e) NULL
              )
            }
          }

          # Initialise step stack with the function body
          rs$step_stack <- list(
            list(type = "fn", exprs = split_body_expressions(.user_fn),
                 pc = 1L, line_map = .line_map)
          )
          rs$paused      <- TRUE
          rs$pause_owner <- .name
          append_log(run_log, paste0("Entered ", .name, "()"), type = "pause")
          # Throw a typed condition so that any enclosing synchronous call
          # (e.g. controller$run() from TrialSimulator) is aborted and control
          # returns to the Shiny event loop.  run_main_until_pause_or_end checks
          # rs$paused before the error field, so this is treated as a pause.
          stop(structure(
            list(message = paste0("shinyStep pause at ", .name)),
            class = c("shinyStep_pause", "error", "condition")
          ))
        }

        # Not a debug target — run silently
        res <- tryCatch(
          do.call(.user_fn, call_args),
          error = function(e) {
            append_log(run_log, paste0(.name, ": ", conditionMessage(e)), type = "error")
            rs$error <- TRUE
            NULL
          }
        )
        invisible(res)
      }

      # Give the proxy the user function's formals (adds "..." if absent)
      .uf_fmls <- formals(.user_fn)
      if (!("..." %in% names(.uf_fmls))) .uf_fmls <- c(.uf_fmls, alist(... = ))
      formals(.proxy) <- .uf_fmls

      rs$env[[.name]] <- .proxy
    })
  }

  rs$main_exprs <- parse_top_level(main_code)
  rs$main_pc    <- 1L
  append_log(run_log, "Runner ready.", type = "info")
}

# ── Public API ───────────────────────────────────────────────────────────────

#' Launch the main program
#'
#' Initialises the runner, installs proxy closures for every registered
#' module whose body is non-blank, and starts executing \code{main_code}.
#' Execution pauses automatically when a function listed in
#' \code{debug_targets} is called.
#'
#' @param runner Runner from \code{make_runner()}.
#' @param main_code Character string — the R program to execute.
#' @param debug_targets Character vector of function names to pause at, or
#'   \code{NULL} (default) to auto-collect from every embedded module whose
#'   Debug checkbox is ticked. Solo modules are never auto-included.
#' @param run_log A \code{reactiveVal} used as the shared log.
#' @export
run_program <- function(runner, main_code,
                        debug_targets = NULL,
                        run_log) {
  if (is.null(debug_targets)) {
    debug_targets <- .collect_debug_targets(runner)
  }
  run_log("")
  .init_runner(runner, main_code, debug_targets, run_log)
  if (!isTRUE(runner$state$error))
    run_main_until_pause_or_end(runner, run_log)
}

# Walk the registry and return fn_names of embedded modules with Debug on.
# Solo modules are excluded — they are only debugged via their own Test
# button, never during a host-driven run.
.collect_debug_targets <- function(runner) {
  reg <- runner$registry
  out <- character(0)
  for (mod_id in ls(reg)) {
    entry <- reg[[mod_id]]
    if (!identical(entry$type, "embedded")) next
    is_on <- isTRUE(tryCatch(entry$enabled(), error = function(e) FALSE))
    if (!is_on) next
    nm <- tryCatch(entry$fn_name(), error = function(e) NULL)
    if (!is.null(nm) && nzchar(nm)) out <- c(out, nm)
  }
  out
}
