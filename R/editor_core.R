# ── editor_core.R ─────────────────────────────────────────────────────────────
# Internal UI + server helpers shared by soloStepUI/Server and
# embeddedStepUI/Server. Nothing here is exported; callers compose these
# pieces into the two public module flavours.

# ── JavaScript ────────────────────────────────────────────────────────────────

.step_js <- function(ns) {
  shiny::tags$script(shiny::HTML(paste0(
    "(function(){$(function(){",
    "(function(){",
    "  var el=document.getElementById('", ns("log_box"), "');",
    "  if(!el) return;",
    "  var obs=new MutationObserver(function(){ el.scrollTop=el.scrollHeight; });",
    "  obs.observe(el,{childList:true,subtree:true,characterData:true});",
    "})();",
    "(function(){",
    "  var _mx=0,_my=0,_ct=null;",
    "  $(document).on('mousedown','#", ns("log_box"), "',function(e){_mx=e.clientX;_my=e.clientY;});",
    "  $(document).on('click','#", ns("log_box"), "',function(e){",
    "    if($(e.target).is('textarea,input,button,a')) return;",
    "    if(Math.abs(e.clientX-_mx)>4||Math.abs(e.clientY-_my)>4) return;",
    "    if(_ct) clearTimeout(_ct);",
    "    _ct=setTimeout(function(){",
    "      _ct=null;",
    "      var ta=document.getElementById('", ns("console_in"), "');",
    "      if(ta){ta.focus();ta.setSelectionRange(ta.value.length,ta.value.length);}",
    "    },250);",
    "  });",
    "  $(document).on('dblclick','#", ns("log_box"), "',function(e){",
    "    if(_ct){clearTimeout(_ct);_ct=null;}",
    "    setTimeout(function(){",
    "      var s=window.getSelection();",
    "      if(s&&s.toString().length>0) document.execCommand('copy');",
    "    },10);",
    "  });",
    "  $(document).on('mouseup','#", ns("log_box"), "',function(e){",
    "    var s=window.getSelection();",
    "    if(s&&s.toString().length>0) document.execCommand('copy');",
    "  });",
    "})();",
    "Shiny.addCustomMessageHandler('", ns("focus_elem"), "',function(msg){",
    "  var tries=0, maxTries=20;",
    "  var tick=function(){",
    "    var el=document.getElementById(msg.id);",
    "    if(el){ el.focus(); if(el.setSelectionRange){",
    "      try{ el.setSelectionRange(el.value.length,el.value.length);}catch(_){}",
    "    } return; }",
    "    if(tries++<maxTries) setTimeout(tick,25);",
    "  };",
    "  tick();",
    "});",
    "Shiny.addCustomMessageHandler('", ns("toggle_controls"), "',function(msg){",
    "  ['", ns("btn_next"), "','", ns("btn_step_out"), "','",
         ns("btn_continue"), "','", ns("btn_stop"), "'].forEach(function(id){",
    "    var el=document.getElementById(id);",
    "    if(el){ el.disabled=msg.disabled;",
    "      el.style.opacity=msg.disabled?'0.45':'';",
    "      el.style.cursor=msg.disabled?'not-allowed':'';",
    "    }",
    "  });",
    "});",
    "var _hl={marker:null,gutter:null};",
    "function _sStepEd(id){",
    "  var el=document.getElementById(id); if(!el) return null;",
    "  return (el.env&&el.env.editor)||el.aceEditor||",
    "         (typeof ace!=='undefined'?ace.edit(el):null);",
    "}",
    "Shiny.addCustomMessageHandler('", ns("highlight_line"), "',function(msg){",
    "  var editor=_sStepEd('", ns("code"), "'); if(!editor) return;",
    "  var session=editor.getSession();",
    "  var Range=ace.require('ace/range').Range;",
    "  if(_hl.marker!==null){session.removeMarker(_hl.marker);_hl.marker=null;}",
    "  if(_hl.gutter!==null){session.removeGutterDecoration(_hl.gutter,'sStep-gutter-arrow');_hl.gutter=null;}",
    "  if(msg.line!==null&&msg.line!==undefined){",
    "    var row=msg.line-1;",
    "    _hl.marker=session.addMarker(new Range(row,0,row,1),'sStep-current-line','fullLine');",
    "    session.addGutterDecoration(row,'sStep-gutter-arrow');",
    "    _hl.gutter=row;",
    "    editor.scrollToLine(row,true,true,function(){});",
    "  }",
    "});",
    "(function(){",
    "  var _h=[],_i=-1;",
    "  $(document).on('keydown','#", ns("console_in"), "',function(e){",
    "    if(e.key==='ArrowUp'){",
    "      e.preventDefault();",
    "      if(_i<_h.length-1){_i++;this.value=_h[_i];}",
    "    } else if(e.key==='ArrowDown'){",
    "      e.preventDefault();",
    "      if(_i>0){_i--;this.value=_h[_i];}",
    "      else if(_i===0){_i=-1;this.value='';}",
    "    } else if(e.key==='Enter'){",
    "      e.preventDefault();",
    "      var v=this.value.trim();",
    "      if(v.length>0){",
    "        _h.unshift(v);_i=-1;",
    "        Shiny.setInputValue('", ns("console_submit"), "',v,{priority:'event'});",
    "        this.value='';",
    "      }",
    "    } else if(e.key==='l'&&e.ctrlKey){",
    "      e.preventDefault();",
    "      Shiny.setInputValue('", ns("clear_log"), "',Date.now(),{priority:'event'});",
    "    }",
    "  });",
    "})();",
    "});})()"
  )))
}

# ── UI fragments ─────────────────────────────────────────────────────────────

.toolbar <- function(ns, label, show_debug, show_test) {
  # `label` kept in the signature for backward compatibility but no longer
  # rendered: parent apps that re-rendered the UI on every keystroke to "sync"
  # the toolbar name caused focus drift, and a duplicate of the fn_name input
  # just below adds no value.  The fn_name input is the source of truth.
  shiny::div(class = "sStep-toolbar",
    shiny::actionButton(ns("back"), label = NULL,
                        icon  = shiny::icon("arrow-left"),
                        class = "btn-sm btn-default",
                        title = "Cancel — discard changes"),
    shiny::actionButton(ns("save"), "Save",
                        icon  = shiny::icon("floppy-disk"),
                        class = "btn-sm btn-success",
                        title = "Save function"),
    shiny::uiOutput(ns("status_badge"), inline = TRUE),
    shiny::div(class = "sStep-toolbar-right",
      if (show_test)
        shiny::actionButton(ns("btn_test"), "Test",
                            icon  = shiny::icon("play"),
                            class = "btn-sm btn-primary",
                            title = "Run this function with the test values below"),
      if (show_debug)
        shiny::checkboxInput(ns("enabled"), label = "Debug", value = TRUE)
    )
  )
}

.args_card <- function(ns, show_test_value) {
  cols <- if (show_test_value) "Name | Default | Test value"
          else                 "Name | Default"
  shiny::div(class = "sStep-args-card",
    shiny::div(class = "sStep-args-title", "Arguments"),
    shiny::div(class = "sStep-args-hint", cols),
    shiny::uiOutput(ns("args_ui")),
    shiny::actionButton(ns("btn_add_arg"), "+ Add argument",
                        class = "btn-xs btn-default sStep-add-arg")
  )
}

.editor_and_debug_pane <- function(ns, height, theme, default_body) {
  shiny::fluidRow(
    shiny::column(6,
      shinyAce::aceEditor(
        outputId        = ns("code"),
        value           = default_body,
        mode            = "r",
        theme           = theme,
        height          = height,
        fontSize        = 14,
        autoComplete    = "live",
        showLineNumbers = TRUE,
        debounce        = 300
      )
    ),
    shiny::column(6,
      shiny::div(
        class = "sStep-right-pane",
        style = paste0("height:", height, ";"),
        shiny::div(class = "sStep-controls",
          shiny::actionButton(ns("btn_next"),     "Next",
                              class = "btn-sm btn-warning"),
          shiny::actionButton(ns("btn_step_out"), "Step Out",
                              class = "btn-sm btn-success",
                              title = "Exit the current loop or if/else block"),
          shiny::actionButton(ns("btn_continue"), "Continue",
                              class = "btn-sm btn-info"),
          shiny::actionButton(ns("btn_stop"),     "Stop",
                              class = "btn-sm btn-danger")
        ),
        shiny::tags$hr(class = "sStep-hr"),
        shiny::div(
          id    = ns("log_box"),
          class = "sStep-terminal",
          shiny::uiOutput(ns("log_ui")),
          shiny::div(class = "sStep-input-line",
            shiny::tags$span(class = "sStep-prompt", ">"),
            shiny::textAreaInput(ns("console_in"), label = NULL, value = "",
                                 rows        = 1,
                                 resize      = "none",
                                 placeholder = "")
          )
        )
      )
    )
  )
}

# Full UI for one module (solo or embedded).
.step_ui <- function(id, label, height, theme, default_body,
                     show_debug, show_test, show_test_value) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "shinyStep/shinyStep.css")
    ),
    .step_js(ns),
    .toolbar(ns, label, show_debug = show_debug, show_test = show_test),
    shiny::div(class = "sStep-name-row",
      shiny::tags$label(class = "sStep-name-label", "Function name"),
      shiny::textInput(ns("fn_name"), label = NULL, value = "",
                       placeholder = "e.g. my_fn", width = "260px")
    ),
    .args_card(ns, show_test_value = show_test_value),
    .editor_and_debug_pane(ns, height, theme, default_body)
  )
}

# ── Server helpers ───────────────────────────────────────────────────────────

.new_arg <- function(name = "", default = "", test_value = "") {
  list(id         = paste0("a", format(Sys.time(), "%H%M%OS6"),
                           sprintf("%04d", sample.int(10000, 1))),
       name       = name,
       default    = default,
       test_value = test_value)
}

.normalise_arg_list <- function(a) {
  if (is.null(a) || length(a) == 0L) return(list())
  lapply(a, function(x) {
    if (is.null(x$id) || !nzchar(x$id)) x <- c(x, list(id = .new_arg()$id))
    list(id         = x$id,
         name       = x$name       %||% "",
         default    = x$default    %||% "",
         test_value = x$test_value %||% "")
  })
}

# Shared server wiring used by both soloStepServer and embeddedStepServer.
# Returns a list of reactives that the caller surfaces to the host app.
#
# Arguments:
#   input, output, session — from moduleServer
#   runner, run_log        — from make_runner()
#   type                   — "solo" | "embedded"
#   initial_fn_name        — character or reactive; default fn name
#   initial_body           — character or reactive; default body
#   initial_args           — list of arg specs, or reactive returning such
#   show_test_value        — TRUE for solo (extra column in args table)
.step_server_core <- function(input, output, session,
                              runner, run_log,
                              type,
                              initial_fn_name,
                              initial_body,
                              initial_args,
                              show_test_value) {
  ns <- session$ns
  rs <- runner$state

  # ── Reactive state ───────────────────────────────────────────────────────
  resolve_ic <- function(x) {
    if (shiny::is.reactive(x)) shiny::isolate(x()) else x
  }
  start_fn_name <- resolve_ic(initial_fn_name) %||% ""
  start_body    <- resolve_ic(initial_body)    %||% ""
  start_args    <- .normalise_arg_list(resolve_ic(initial_args))

  # Do NOT derive a fallback fn_name from the module id. If the parent app
  # passes "" that's a deliberate signal — e.g. a fresh "+ New" entry whose
  # name will be assigned on save or back. A derived fallback here would
  # silently fill the name and break parent-side "is this truly empty?"
  # checks (observed in TrialDesigner: + New followed by Back left behind a
  # stray generator because the auto-save wrote the derived id as a name).
  start_fn_name <- trimws(start_fn_name)

  fn_name_rv <- shiny::reactiveVal(start_fn_name)
  body_rv    <- shiny::reactiveVal(start_body)
  args_rv    <- shiny::reactiveVal(start_args)
  # Structure-only reactive: invalidates when the set of row ids changes, not
  # when row contents change. Keeps renderUI from rebuilding (and stealing
  # focus) on every keystroke.
  args_ids_rv <- shiny::reactiveVal(
    vapply(start_args, `[[`, character(1L), "id")
  )
  modified_rv <- shiny::reactiveVal(FALSE)

  # Seed the fn_name textInput from state (first-render and whenever initial
  # is a reactive that changes).
  # Guard each observer with an identical() check against current *_rv: when a
  # parent app auto-saves back into the same reactive we read from, the
  # round-trip value is our own echo.  Pushing it to the client via
  # updateTextInput / updateAceEditor / args_ids_rv would reset the user's
  # cursor, wipe unsaved ace selections, and tear down the args DOM — exactly
  # the focus-drift bug observers see when a reactive initial_* is fed by a
  # central store that also receives our saved state.
  shiny::updateTextInput(session, "fn_name", value = start_fn_name)
  if (shiny::is.reactive(initial_fn_name)) {
    shiny::observeEvent(initial_fn_name(), {
      val <- initial_fn_name() %||% ""
      if (identical(val, shiny::isolate(fn_name_rv()))) return()
      if (nzchar(val)) {
        fn_name_rv(val)
        shiny::updateTextInput(session, "fn_name", value = val)
      }
    }, ignoreInit = FALSE)
  }
  if (shiny::is.reactive(initial_body)) {
    shiny::observeEvent(initial_body(), {
      val <- initial_body() %||% ""
      if (identical(val, shiny::isolate(body_rv()))) return()
      body_rv(val)
      shinyAce::updateAceEditor(session, "code", value = val)
    }, ignoreInit = FALSE)
  }
  if (shiny::is.reactive(initial_args)) {
    shiny::observeEvent(initial_args(), {
      new_args <- .normalise_arg_list(initial_args())
      if (identical(new_args, shiny::isolate(args_rv()))) return()
      args_rv(new_args)
      new_ids <- vapply(new_args, `[[`, character(1L), "id")
      # Only refresh args_ids_rv when the row structure changes — content-only
      # edits must not trigger renderUI or the user loses focus mid-type.
      if (!identical(new_ids, shiny::isolate(args_ids_rv())))
        args_ids_rv(new_ids)
    }, ignoreInit = FALSE)
  }

  # ── Registry registration ────────────────────────────────────────────────
  # Key by ns-stripped module id so solo/embedded never collide.
  mod_id <- sub("-$", "", session$ns(""))
  runner$registry[[mod_id]] <- list(
    type     = type,
    fn_name  = function() shiny::isolate(fn_name_rv()),
    get_body = function() shiny::isolate(body_rv()),
    get_args = function() shiny::isolate(args_rv()),
    enabled  = if (type == "embedded")
                 function() isTRUE(shiny::isolate(input$enabled))
               else
                 function() FALSE
  )
  session$onSessionEnded(function() {
    if (exists(mod_id, envir = runner$registry))
      rm(list = mod_id, envir = runner$registry)
  })

  # ── Editor ↔ body_rv ─────────────────────────────────────────────────────
  shiny::observeEvent(input$code, {
    body_rv(input$code %||% "")
    fn_name <- shiny::isolate(fn_name_rv())
    if (isTRUE(rs$paused) && identical(rs$pause_owner, fn_name))
      modified_rv(TRUE)
  }, ignoreInit = FALSE)

  # ── fn_name input ↔ fn_name_rv ───────────────────────────────────────────
  shiny::observeEvent(input$fn_name, {
    nm <- trimws(input$fn_name %||% "")
    if (nzchar(nm)) fn_name_rv(nm)
  }, ignoreInit = FALSE)

  # ── Args state machine ───────────────────────────────────────────────────
  # Snapshot any currently-typed values from the DOM into args_rv. Called
  # before Add / Remove so pending keystrokes aren't lost when the UI rebuilds.
  .snapshot_args <- function() {
    cur <- shiny::isolate(args_rv())
    if (length(cur) == 0L) return(invisible())
    changed <- FALSE
    for (i in seq_along(cur)) {
      a  <- cur[[i]]
      nm <- input[[paste0("arg_name_",    a$id)]]
      df <- input[[paste0("arg_default_", a$id)]]
      tv <- if (show_test_value) input[[paste0("arg_testval_", a$id)]] else NULL
      if (!is.null(nm) && !identical(nm, a$name))       { cur[[i]]$name    <- nm; changed <- TRUE }
      if (!is.null(df) && !identical(df, a$default))    { cur[[i]]$default <- df; changed <- TRUE }
      if (show_test_value && !is.null(tv) &&
          !identical(tv, a$test_value))                 { cur[[i]]$test_value <- tv; changed <- TRUE }
    }
    if (changed) args_rv(cur)
    invisible()
  }

  # RenderUI depends ONLY on args_ids_rv — which changes on add/remove but not
  # on typing — so the DOM inputs are not torn down while the user is editing.
  output$args_ui <- shiny::renderUI({
    ids <- args_ids_rv()
    args <- shiny::isolate(args_rv())
    if (length(ids) == 0L) {
      return(shiny::div(class = "sStep-args-empty",
                        "No arguments. Click + Add argument below."))
    }
    shiny::tagList(lapply(args, function(a) {
      shiny::div(class = "sStep-arg-row",
        shiny::textInput(ns(paste0("arg_name_", a$id)), label = NULL,
                         value = a$name, width = "140px",
                         placeholder = "name"),
        shiny::textInput(ns(paste0("arg_default_", a$id)), label = NULL,
                         value = a$default, width = "160px",
                         placeholder = "default (optional)"),
        if (show_test_value)
          shiny::textInput(ns(paste0("arg_testval_", a$id)), label = NULL,
                           value = a$test_value, width = "160px",
                           placeholder = "test value"),
        shiny::actionButton(ns(paste0("rm_arg_", a$id)), label = NULL,
                            icon = shiny::icon("xmark"),
                            class = "btn-xs btn-default sStep-arg-del",
                            title = "Remove argument",
                            tabindex = "-1")
      )
    }))
  })

  shiny::observeEvent(input$btn_add_arg, {
    .snapshot_args()
    shiny::isolate({
      cur     <- args_rv()
      new_row <- .new_arg()
      cur     <- c(cur, list(new_row))
      args_rv(cur)
      args_ids_rv(vapply(cur, `[[`, character(1L), "id"))
    })
    session$sendCustomMessage(ns("focus_elem"),
                              list(id = ns(paste0("arg_name_", new_row$id))))
  }, ignoreInit = TRUE)

  # Continuous input-sync: writes typed values into args_rv. Does NOT invalidate
  # args_ids_rv (structure unchanged), so renderUI does not rebuild.
  shiny::observe({
    ids <- args_ids_rv()
    if (length(ids) == 0L) return()
    for (id in ids) {
      input[[paste0("arg_name_",    id)]]
      input[[paste0("arg_default_", id)]]
      if (show_test_value) input[[paste0("arg_testval_", id)]]
    }
    shiny::isolate(.snapshot_args())
  })

  # Remove-row buttons — one observer per current id, re-registered when the
  # structure changes.
  shiny::observe({
    ids <- args_ids_rv()
    lapply(ids, function(id) {
      btn_id <- paste0("rm_arg_", id)
      shiny::observeEvent(input[[btn_id]], {
        .snapshot_args()
        shiny::isolate({
          cur  <- args_rv()
          keep <- vapply(cur, function(a) a$id != id, logical(1L))
          cur  <- cur[keep]
          args_rv(cur)
          args_ids_rv(vapply(cur, `[[`, character(1L), "id"))
        })
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Reset modified flag when a new run starts
  shiny::observe({
    rs$running
    if (isTRUE(rs$running)) modified_rv(FALSE)
  })

  # ── Status badge ─────────────────────────────────────────────────────────
  output$status_badge <- shiny::renderUI({
    fn_name <- fn_name_rv()
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

  # ── Editor line highlight ────────────────────────────────────────────────
  shiny::observe({
    fn_name     <- fn_name_rv()
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
      lm  <- top$line_map
      if (!is.null(lm) && pc >= 1L && pc <= length(lm) && !is.na(lm[[pc]])) {
        line <- as.integer(lm[[pc]])
      } else {
        expr <- current_expr(rs)
        if (!is.null(expr)) {
          sr <- attr(expr, "srcref")
          if (!is.null(sr)) line <- as.integer(sr[1L])
        }
      }
    }
    # Map assembled-code line (1 = `fn <- function(...) {`, 2 = first body line)
    # back to editor-visible line by subtracting the header offset (1).
    if (!is.null(line)) line <- max(1L, line - 1L)
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

  # ── Step-control enable/disable ──────────────────────────────────────────
  shiny::observe({
    fn_name  <- fn_name_rv()
    disabled <- !(isTRUE(rs$paused) && identical(rs$pause_owner, fn_name))
    session$sendCustomMessage(ns("toggle_controls"), list(disabled = disabled))
  })

  .paused_here <- function() {
    isTRUE(rs$paused) && identical(rs$pause_owner, shiny::isolate(fn_name_rv()))
  }

  shiny::observeEvent(input$btn_next, {
    shiny::req(.paused_here())
    step_fn(runner, run_log)
  })
  shiny::observeEvent(input$btn_step_out, {
    shiny::req(.paused_here())
    step_out_frame(runner, run_log)
  })
  shiny::observeEvent(input$btn_continue, {
    shiny::req(.paused_here())
    continue_to_next_pause(runner, run_log)
  })
  shiny::observeEvent(input$btn_stop, {
    shiny::req(isTRUE(rs$running) || isTRUE(rs$paused))
    stop_runner(runner, run_log)
  })

  # ── Console evaluation ──────────────────────────────────────────────────
  # Evaluate every top-level expression in code_txt, not just the first. This
  # lets users paste multi-line snippets and have them run sequentially, like
  # a real R console. On parse failure the whole submission is reported as an
  # error; on eval error we stop at that expression (matching `source()` /
  # REPL semantics).
  .run_console <- function(code_txt) {
    shiny::req(.paused_here())
    if (!nzchar(trimws(code_txt %||% ""))) return()
    append_log(run_log, code_txt, type = "console")
    exprs <- tryCatch(parse(text = code_txt),
                      error = function(e) e)
    if (inherits(exprs, "error")) {
      append_log(run_log, conditionMessage(exprs), type = "error")
      return()
    }
    for (i in seq_along(exprs)) {
      res <- eval_with_capture(exprs[[i]], envir = rs$fun_frame, auto_print = TRUE)
      if (nzchar(res$output)) append_log(run_log, res$output)
      if (!is.null(res$error)) {
        append_log(run_log, conditionMessage(res$error), type = "error")
        break
      }
    }
  }
  shiny::observeEvent(input$console_submit, { .run_console(input$console_submit) },
                      ignoreInit = TRUE)
  shiny::observeEvent(input$clear_log, { run_log("") }, ignoreInit = TRUE)

  # ── Return handles ───────────────────────────────────────────────────────
  list(
    back_clicked = shiny::reactive(input$back),
    save_clicked = shiny::reactive(input$save),
    fn_name      = shiny::reactive(fn_name_rv()),
    body         = shiny::reactive(body_rv()),
    args         = shiny::reactive(args_rv()),
    get_body     = function() shiny::isolate(body_rv()),
    get_args     = function() shiny::isolate(args_rv()),
    get_fn_name  = function() shiny::isolate(fn_name_rv()),
    enabled      = if (type == "embedded")
                     shiny::reactive(isTRUE(input$enabled))
                   else
                     shiny::reactive(FALSE)
  )
}
