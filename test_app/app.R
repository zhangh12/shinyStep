library(shiny)
library(shinyStep)

# Demo showcasing both shinyStep debugging modes:
#
#   • Solo mode (left) — test a regular function in isolation. Supply test
#     values for each argument, click Test, step through the body.
#
#   • Embedded mode (right) — define a function that is invoked by a larger
#     "main program" the host app assembles. Tick Debug, edit main_code,
#     click Run Program, and pause at the first line of the function every
#     time main_code calls it.
#
# Both share one runner and one run_log.

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
  titlePanel("shinyStep demo — solo and embedded modes"),

  # ── Package prelude ──────────────────────────────────────────────────────
  wellPanel(
    div(style = "display:flex; gap:10px; align-items:flex-end;",
      div(style = "flex:1;",
        tags$label("Packages to load before every run (comma- or space-separated)"),
        textInput("packages", label = NULL,
                  value       = "",
                  width       = "100%",
                  placeholder = "e.g. dplyr, ggplot2")
      ),
      uiOutput("packages_status")
    ),
    helpText("These are prepended as library(pkg) calls to every Solo Test ",
             "and to the Embedded Run Program. Loaded into the runner's ",
             "execution environment on each run.")
  ),

  fluidRow(

    # ── Solo column ────────────────────────────────────────────────────────
    column(6,
      h3("Solo: sum_vec()"),
      helpText("Tests a single function in isolation. Fill in test values, ",
               "click ", strong("Test"), ", step through with Next."),
      soloStepUI("sum_vec", label = "sum_vec", height = "360px",
                 default_body = solo_default_body)
    ),

    # ── Embedded column ────────────────────────────────────────────────────
    column(6,
      h3("Embedded: greet()"),
      helpText("Declared here but called by the main program below. ",
               "Tick ", strong("Debug"), " and click ", strong("Run Program"),
               "to pause at the first line of greet() on every call."),
      embeddedStepUI("greet", label = "greet", height = "360px",
                     default_body = embedded_default_body),
      wellPanel(
        h4("Main program"),
        textAreaInput("main_code", label = NULL, value = default_main_code,
                      rows = 6, width = "100%"),
        actionButton("run_btn", "Run Program",
                     icon  = icon("play"),
                     class = "btn-primary")
      )
    )
  )
)

server <- function(input, output, session) {

  runner  <- make_runner()
  run_log <- reactiveVal("")

  # ── Prelude: "library(pkg1)\nlibrary(pkg2)\n..." from the packages input ─
  prelude <- reactive({
    raw <- trimws(input$packages %||% "")
    if (!nzchar(raw)) return("")
    pkgs <- unlist(strsplit(raw, "[,\\s]+", perl = TRUE))
    pkgs <- pkgs[nzchar(pkgs)]
    if (length(pkgs) == 0L) return("")
    paste(sprintf("library(%s)", pkgs), collapse = "\n")
  })

  output$packages_status <- renderUI({
    pkgs <- unlist(strsplit(trimws(input$packages %||% ""), "[,\\s]+", perl = TRUE))
    pkgs <- pkgs[nzchar(pkgs)]
    if (length(pkgs) == 0L)
      tags$span(style = "color:#9098b0; font-size:12px;", "(none)")
    else
      tags$span(style = "color:#2e7d32; font-size:12px; font-weight:600;",
                paste0(length(pkgs), " package",
                       if (length(pkgs) == 1L) "" else "s"))
  })

  # ── Solo: sum_vec(x) ────────────────────────────────────────────────────
  soloStepServer(
    "sum_vec",
    runner  = runner,
    run_log = run_log,
    initial_fn_name = "sum_vec",
    initial_args = list(
      list(name = "x", default = "", test_value = "1:5")
    ),
    prelude = prelude
  )

  # ── Embedded: greet(who, excited = FALSE) ───────────────────────────────
  embeddedStepServer(
    "greet",
    runner  = runner,
    run_log = run_log,
    initial_fn_name = "greet",
    initial_args = list(
      list(name = "who",     default = ""),
      list(name = "excited", default = "FALSE")
    )
  )

  # ── Run the embedded main program ───────────────────────────────────────
  observeEvent(input$run_btn, {
    code <- input$main_code %||% ""
    pre  <- prelude()
    if (nzchar(pre)) code <- paste0(pre, "\n", code)
    run_program(
      runner    = runner,
      main_code = code,
      # debug_targets = NULL → auto-collect from embedded modules whose
      # Debug checkbox is ticked. Solo modules are never included here.
      run_log   = run_log
    )
  })
}

shinyApp(ui, server)
