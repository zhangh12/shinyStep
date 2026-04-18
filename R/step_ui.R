# ── JavaScript helpers ───────────────────────────────────────────────────────

.step_js <- function(ns) {
  shiny::tags$script(shiny::HTML(paste0(
    "(function(){$(function(){",
    # Auto-scroll via MutationObserver — fires after DOM paint, not before
    "(function(){",
    "  var el=document.getElementById('", ns("log_box"), "');",
    "  if(!el) return;",
    "  var obs=new MutationObserver(function(){ el.scrollTop=el.scrollHeight; });",
    "  obs.observe(el,{childList:true,subtree:true,characterData:true});",
    "})();",
    # Click anywhere in the terminal → focus the input textarea.
    # Track mousedown position so we don't steal focus during drag-select.
    # Delay focus by 250 ms so a dblclick can cancel it and select a word.
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
    # Green-arrow line highlight (RStudio-style)
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
    # Terminal prompt: Enter submits, Up/Down navigate history
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

# ── Module UI ────────────────────────────────────────────────────────────────

#' UI for a shinyStep debugger module
#'
#' Place one call to \code{stepUI()} in your Shiny UI for each function you
#' want to be able to debug. Pair with \code{stepServer()} in the server.
#'
#' @param id Module namespace ID. Must match the \code{id} passed to
#'   \code{stepServer()}.
#' @param label Display label shown in the toolbar (e.g. the function name).
#' @param height Ace editor height as a CSS string. Default \code{"500px"}.
#' @param theme Ace editor theme. Default \code{"textmate"}.
#' @param default_code Initial code placed in the editor when no
#'   \code{initial_code} is supplied to \code{stepServer()}.
#'
#' @return A \code{tagList} suitable for inclusion anywhere in a Shiny UI.
#' @export
stepUI <- function(id,
                   label        = id,
                   height       = "500px",
                   theme        = "textmate",
                   default_code = "function() {\n  \n}") {
  ns <- shiny::NS(id)

  shiny::tagList(
    # CSS (loaded once via addResourcePath in .onLoad)
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "shinyStep/shinyStep.css")
    ),
    # Inline JS for this module instance
    .step_js(ns),

    # ── Toolbar ─────────────────────────────────────────────────────────────
    shiny::div(class = "sStep-toolbar",
      shiny::actionButton(ns("back"), label = NULL,
                          icon = shiny::icon("arrow-left"),
                          class = "btn-sm btn-default",
                          title = "Back"),
      shiny::span(class = "sStep-title", label),
      shiny::uiOutput(ns("status_badge"), inline = TRUE),
      shiny::div(class = "sStep-toolbar-right",
        shiny::checkboxInput(ns("enabled"), label = "Debug", value = TRUE)
      )
    ),

    # ── Main two-column layout ───────────────────────────────────────────────
    shiny::fluidRow(

      # Left: Ace editor
      shiny::column(6,
        shinyAce::aceEditor(
          outputId        = ns("code"),
          value           = default_code,
          mode            = "r",
          theme           = theme,
          height          = height,
          fontSize        = 14,
          autoComplete    = "live",
          showLineNumbers = TRUE,
          debounce        = 300
        )
      ),

      # Right: debug controls pane
      shiny::column(6,
        shiny::div(
          class = "sStep-right-pane",
          style = paste0("height:", height, ";"),

          # Step controls
          shiny::div(class = "sStep-controls",
            shiny::actionButton(ns("btn_next"),      "Next",
                                class = "btn-sm btn-warning"),
            shiny::actionButton(ns("btn_step_out"),  "Step Out",
                                class = "btn-sm btn-success",
                                title = "Exit the current loop or if/else block"),
            shiny::actionButton(ns("btn_continue"),  "Continue",
                                class = "btn-sm btn-info"),
            shiny::actionButton(ns("btn_stop"),      "Stop",
                                class = "btn-sm btn-danger")
          ),

          shiny::tags$hr(class = "sStep-hr"),

          # Single unified terminal — output + prompt in one scrollable box
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
  )
}
