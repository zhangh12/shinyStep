library(shiny)
library(shinyStep)

# Demo showcasing both shinyStep debugging modes:
#
#   • Solo  — test a regular function in isolation via its own Test button.
#   • Embedded — define a function called by a larger host program; tick
#     Debug and click Run Program to pause at its first line.
#
# The left sidebar switches between the Functions editor view and the
# Packages configuration panel.  Both module servers always run; sections
# are shown/hidden with JS so Ace editors are never initialized while hidden.

solo_default_body <- paste(
  "total <- 0",
  "for (v in x) {",
  "  total <- total + v",
  "}",
  "total",
  sep = "\n"
)

embedded_default_body <- paste(
  "msg <- paste('Hello,', who)",
  "if (excited) msg <- paste0(msg, '!')",
  "cat(msg, '\\n')",
  "invisible(msg)",
  sep = "\n"
)

default_main_code <- paste(
  "greet('world', excited = TRUE)",
  "greet('shinyStep')",
  sep = "\n"
)

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      /* ── Left sidebar ─────────────────────────────────────────────── */
      .demo-nav {
        padding: 8px 0;
        border-right: 1px solid #dde3ee;
        min-height: 600px;
      }
      .demo-nav-btn {
        display: block;
        width: 100%;
        padding: 7px 8px;
        text-align: left;
        background: none;
        border: none;
        border-left: 3px solid transparent;
        font-size: 13px;
        color: #444;
        cursor: pointer;
        transition: background 0.15s;
      }
      .demo-nav-btn:hover  { background: #f0f4ff; }
      .demo-nav-btn.active {
        background: #eef2ff;
        border-left-color: #4a6cf7;
        color: #2a3fa0;
        font-weight: 600;
      }
    ")),
    tags$script(HTML("
      var DEMO_SECTIONS = ['functions', 'packages'];

      function demoShow(sec) {
        DEMO_SECTIONS.forEach(function(s) {
          var el = document.getElementById('demo_sec_' + s);
          if (el) el.style.display = (s === sec) ? '' : 'none';
          var btn = document.getElementById('demo_btn_' + s);
          if (btn) {
            if (s === sec) btn.classList.add('active');
            else           btn.classList.remove('active');
          }
        });
      }

      Shiny.addCustomMessageHandler('demoShow', function(sec) { demoShow(sec); });
    "))
  ),

  fluidRow(

    # ── Left sidebar ───────────────────────────────────────────────────────
    column(1,
      div(class = "demo-nav",
        tags$button("Functions", id = "demo_btn_functions",
                    class   = "demo-nav-btn active",
                    onclick = "Shiny.setInputValue('demo_nav','functions',{priority:'event'})"),
        tags$button("Packages", id = "demo_btn_packages",
                    class   = "demo-nav-btn",
                    onclick = "Shiny.setInputValue('demo_nav','packages',{priority:'event'})")
      )
    ),

    # ── Main content ───────────────────────────────────────────────────────
    column(11,

      # ── Functions section ──────────────────────────────────────────────
      div(id = "demo_sec_functions",
        tabsetPanel(

          tabPanel("Solo",
            br(),
            helpText("Tests a single function in isolation. Fill in test values, ",
                     "click ", strong("Test"), ", then step through with Next."),
            soloStepUI("sum_vec", height = "360px",
                       default_body    = solo_default_body,
                       default_fn_name = "sum_vec")
          ),

          tabPanel("Embedded",
            br(),
            helpText("Declared here but called by the main program below. ",
                     "Tick ", strong("Debug"), " then click ",
                     strong("Run Program"), " to pause at greet()'s first line."),
            embeddedStepUI("greet", height = "360px",
                           default_body    = embedded_default_body,
                           default_fn_name = "greet"),
            wellPanel(
              div(style = "display:flex; align-items:center; gap:12px; margin-bottom:8px;",
                h4("Main program", style = "margin:0;"),
                actionButton("run_btn", "Run Program",
                             icon = icon("play"), class = "btn-primary btn-sm")
              ),
              textAreaInput("main_code", label = NULL,
                            value = default_main_code, rows = 5, width = "100%")
            )
          )
        )
      ),

      # ── Packages section ───────────────────────────────────────────────
      div(id = "demo_sec_packages", style = "display:none;",
        br(),
        wellPanel(
          h4("Packages"),
          helpText("Packages listed here are prepended as ",
                   code("library(pkg)"), " calls to every Solo Test and to ",
                   "the Embedded Run Program."),
          div(style = "display:flex; gap:10px; align-items:flex-end;",
            div(style = "flex:1;",
              tags$label("Packages (comma- or space-separated)"),
              textInput("packages", label = NULL, value = "",
                        width = "100%", placeholder = "e.g. dplyr, ggplot2")
            ),
            uiOutput("packages_status")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  runner           <- make_runner()
  solo_run_log     <- reactiveVal("")
  embedded_run_log <- reactiveVal("")

  # ── Sidebar navigation ───────────────────────────────────────────────────
  observeEvent(input$demo_nav, {
    session$sendCustomMessage("demoShow", input$demo_nav)
  }, ignoreInit = TRUE)

  # ── Prelude from packages input ──────────────────────────────────────────
  prelude <- reactive({
    raw  <- trimws(input$packages %||% "")
    if (!nzchar(raw)) return("")
    pkgs <- pkgs <- unlist(strsplit(raw, "[,\\s]+", perl = TRUE))
    pkgs <- pkgs[nzchar(pkgs)]
    if (length(pkgs) == 0L) return("")
    paste(sprintf("library(%s)", pkgs), collapse = "\n")
  })

  output$packages_status <- renderUI({
    pkgs <- unlist(strsplit(trimws(input$packages %||% ""), "[,\\s]+", perl = TRUE))
    pkgs <- pkgs[nzchar(pkgs)]
    if (length(pkgs) == 0L)
      tags$span(style = "color:#9098b0; font-size:13px;", "(none loaded)")
    else
      tags$span(style = "color:#2e7d32; font-size:13px; font-weight:600;",
                paste0(length(pkgs), " package",
                       if (length(pkgs) == 1L) "" else "s"))
  })

  # ── Solo module ──────────────────────────────────────────────────────────
  soloStepServer(
    "sum_vec",
    runner          = runner,
    run_log         = solo_run_log,
    initial_fn_name = "sum_vec",
    initial_args    = list(
      list(name = "x", default = "", test_value = "1:5")
    ),
    prelude = prelude
  )

  # ── Embedded module ──────────────────────────────────────────────────────
  embeddedStepServer(
    "greet",
    runner          = runner,
    run_log         = embedded_run_log,
    initial_fn_name = "greet",
    initial_args    = list(
      list(name = "who",     default = ""),
      list(name = "excited", default = "FALSE")
    )
  )

  # ── Run the embedded main program ────────────────────────────────────────
  observeEvent(input$run_btn, {
    code <- input$main_code %||% ""
    pre  <- prelude()
    if (nzchar(pre)) code <- paste0(pre, "\n", code)
    run_program(
      runner    = runner,
      main_code = code,
      run_log   = embedded_run_log
    )
  })
}

shinyApp(ui, server)
