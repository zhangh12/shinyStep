# ── Null-coalescing ──────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (is.null(x)) y else x

# ── Expression splitting ─────────────────────────────────────────────────────

# Top-level expressions from a function object's body.
split_body_expressions <- function(fn) {
  b <- body(fn)
  if (is.call(b) && identical(b[[1L]], as.name("{")))
    as.list(b)[-1L]
  else
    list(b)
}

# Sub-expressions from a { block or a bare single expression.
split_body_from_expr <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("{")))
    as.list(expr)[-1L]
  else
    list(expr)
}

# Parse a code string into a list of top-level expressions.
parse_top_level <- function(code) {
  if (is.null(code) || !nzchar(trimws(code))) return(list())
  tryCatch(
    as.list(parse(text = code)),
    error = function(e) structure(
      list(call("stop", paste("Parse error:", conditionMessage(e)))),
      parse_error = TRUE,
      parse_msg   = conditionMessage(e)
    )
  )
}

# ── Expression type detection ────────────────────────────────────────────────

is_for_loop    <- function(e) is.call(e) && identical(e[[1L]], as.name("for"))
is_while_loop  <- function(e) is.call(e) && identical(e[[1L]], as.name("while"))
is_repeat_loop <- function(e) is.call(e) && identical(e[[1L]], as.name("repeat"))
is_if_expr     <- function(e) is.call(e) && identical(e[[1L]], as.name("if"))

is_compound <- function(e) {
  is_for_loop(e) || is_while_loop(e) || is_repeat_loop(e) || is_if_expr(e)
}

is_break_expr <- function(e) {
  (is.name(e)  && identical(e, as.name("break"))) ||
  (is.call(e)  && identical(e[[1L]], as.name("break")))
}

is_next_expr <- function(e) {
  (is.name(e) && identical(e, as.name("next"))) ||
  (is.call(e) && identical(e[[1L]], as.name("next")))
}

is_return_expr <- function(e) {
  is.call(e) && identical(e[[1L]], as.name("return"))
}

# ── Evaluation with captured output ─────────────────────────────────────────

# Evaluate expr in envir; capture stdout + messages; return list(value, error, output).
eval_with_capture <- function(expr, envir, auto_print = FALSE) {
  log_var  <- character(0)
  con      <- textConnection("log_var", open = "w", local = TRUE)
  m_before <- sink.number(type = "message")
  o_before <- sink.number(type = "output")
  err      <- NULL

  sink(con, type = "output")
  sink(con, type = "message")

  on.exit({
    while (sink.number(type = "message") > m_before) sink(type = "message")
    while (sink.number(type = "output")  > o_before) sink(type = "output")
    if (isOpen(con)) close(con)
  }, add = TRUE)

  val <- tryCatch({
    vv <- withVisible(eval(expr, envir = envir))
    if (auto_print && vv$visible) print(vv$value)
    vv$value
  }, error = function(e) { err <<- e; NULL })

  output <- tryCatch(
    paste(get("log_var", envir = environment(), inherits = FALSE), collapse = "\n"),
    error = function(e) ""
  )

  list(value = val, error = err, output = output)
}

# ── Shared log helpers ───────────────────────────────────────────────────────

# Append a line to the shared run_log reactiveVal.
# type: "output" | "error" | "pause" | "resume" | "done" | "info" | "console"
append_log <- function(run_log, txt, type = "output") {
  prefix <- switch(type,
    error   = "[Error] ",
    pause   = "[Paused] ",
    resume  = "[Resume] ",
    done    = "[Done] ",
    info    = "[Info] ",
    console = "> ",
    ""
  )
  cur <- run_log()
  run_log(paste0(cur, if (nzchar(cur)) "\n" else "", prefix, txt))
}

# ── Nested body line numbers from getParseData ────────────────────────────────

# Given getParseData output and the start line of a compound expression
# (for/while/repeat/if), return the start lines of the body's direct statements.
# branch_idx: 1 = first body (then-branch or loop body), 2 = else-branch.
.compound_body_lines_from_pd <- function(pd, start_line, branch_idx = 1L) {
  if (is.null(pd) || nrow(pd) == 0L ||
      is.null(start_line) || is.na(start_line)) return(NULL)

  # Find the widest non-terminal expression starting at start_line
  cands <- pd[pd$line1 == start_line & !pd$terminal, , drop = FALSE]
  if (nrow(cands) == 0L) return(NULL)
  cands <- cands[order(cands$line2 - cands$line1, decreasing = TRUE), , drop = FALSE]

  # When multiple non-terminals span the same range (e.g. `for(...) {` puts the
  # for-expression and its body-block on the same line with identical extents),
  # prefer the one that directly owns a compound keyword token — that is the
  # actual for/while/if/repeat expression, not its body block.
  max_span <- cands$line2[1L] - cands$line1[1L]
  tied     <- cands[cands$line2 - cands$line1 == max_span, , drop = FALSE]
  if (nrow(tied) > 1L) {
    kw <- c("FOR", "WHILE", "IF", "REPEAT")
    has_kw <- vapply(tied$id, function(cid)
      any(pd$parent == cid & pd$terminal & pd$token %in% kw),
      logical(1L))
    if (any(has_kw)) tied <- tied[has_kw, , drop = FALSE]
  }
  cand_id <- tied$id[1L]

  # Among non-terminal children, collect those that own a '{' token (body blocks)
  kids <- pd[pd$parent == cand_id & !pd$terminal, , drop = FALSE]
  kids <- kids[order(kids$id), , drop = FALSE]

  body_ids <- integer(0)
  for (j in seq_len(nrow(kids))) {
    kid_id <- kids$id[j]
    if (any(pd$parent == kid_id & pd$token == "'{'" & pd$terminal)) {
      body_ids <- c(body_ids, kid_id)
    }
  }

  if (length(body_ids) < branch_idx) {
    # else-if: the else branch is a non-{ expression (e.g. another `if`).
    # Return its start line so the caller can build a single-entry line_map.
    non_body <- kids[!kids$id %in% body_ids, , drop = FALSE]
    if (nrow(non_body) > 0L) {
      else_line <- pd$line1[pd$id == non_body$id[nrow(non_body)]]
      if (length(else_line) > 0L) return(as.integer(else_line[[1L]]))
    }
    return(NULL)
  }
  body_id <- body_ids[[branch_idx]]

  stmts <- pd[pd$parent == body_id & !pd$terminal, , drop = FALSE]
  stmts <- stmts[order(stmts$id), , drop = FALSE]
  if (nrow(stmts) == 0L) return(NULL)
  as.integer(stmts$line1)
}

# Fallback: extract start lines from srcref on each expression.
# Returns NULL when all srcrefs are absent.
.exprs_line_map <- function(exprs) {
  lines <- vapply(exprs, function(e) {
    sr <- attr(e, "srcref")
    if (!is.null(sr)) as.integer(sr[1L]) else NA_integer_
  }, integer(1L))
  if (all(is.na(lines))) NULL else lines
}

# ── Line-number extraction for editor highlight ───────────────────────────────

# Parse a function code string and return the start line (1-indexed) of each
# top-level body statement.  Returns NULL on any failure.
parse_body_lines <- function(code_txt) {
  p <- tryCatch(
    parse(text = code_txt, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(p) || length(p) == 0L) return(NULL)

  pd <- tryCatch(
    utils::getParseData(p, includeText = FALSE),
    error = function(e) NULL
  )
  if (is.null(pd) || nrow(pd) == 0L) return(NULL)

  lbrace_rows <- pd[pd$token == "'{'" & pd$terminal, , drop = FALSE]

  if (nrow(lbrace_rows) == 0L) {
    # Single-expression body (no braces) — return one line number
    top_ids <- pd[pd$parent == 0L & !pd$terminal, "id", drop = TRUE]
    if (length(top_ids) == 0L) return(NULL)
    fn_expr_id <- top_ids[[length(top_ids)]]
    children   <- pd[pd$parent == fn_expr_id & !pd$terminal, , drop = FALSE]
    if (nrow(children) == 0L) return(NULL)
    body_row <- children[nrow(children), , drop = FALSE]
    return(as.integer(body_row$line1))
  }

  # Body block id is the parent of '{'
  body_block_id <- lbrace_rows$parent[1L]

  # Direct non-terminal children are the statements
  stmt_rows <- pd[pd$parent == body_block_id & !pd$terminal, , drop = FALSE]
  stmt_rows <- stmt_rows[order(stmt_rows$id), , drop = FALSE]

  if (nrow(stmt_rows) == 0L) return(integer(0))
  as.integer(stmt_rows$line1)
}
