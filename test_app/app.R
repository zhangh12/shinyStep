library(shiny)
library(shinyStep)

# ── Initial function code ────────────────────────────────────────────────────
#
# Three chained functions — each result feeds the next:
#   raw  -->  normalize()  -->  normed
#   normed  -->  classify()  -->  labels
#   labels  -->  tally()  (prints summary, returns counts invisibly)
#
# Features exercised:
#   normalize : if with early return(), return value fixup (normed <- normalize(...))
#   classify  : while loop, nested if/else chain, return value fixup
#   tally     : two sequential for loops, nested if, cat() output

INIT_NORMALIZE <- '
normalize <- function(x) {
  mn  <- min(x)
  mx  <- max(x)
  rng <- mx - mn
  if (rng == 0) {
    cat("All values identical — returning zeros.\\n")
    return(numeric(length(x)))
  }
  (x - mn) / rng
}
'

INIT_CLASSIFY <- '
classify <- function(x, breaks = c(0.33, 0.67)) {
  out <- character(length(x))
  i   <- 1L
  while (i <= length(x)) {
    v <- x[i]
    if (v <= breaks[1]) {
      out[i] <- "low"
    } else if (v <= breaks[2]) {
      out[i] <- "mid"
    } else {
      out[i] <- "high"
    }
    i <- i + 1L
  }
  out
}
'

INIT_TALLY <- '
tally <- function(labels) {
  lvls   <- c("low", "mid", "high")
  counts <- setNames(integer(length(lvls)), lvls)
  for (lbl in labels) {
    if (lbl %in% lvls) {
      counts[lbl] <- counts[lbl] + 1L
    }
  }
  cat("--- tally ---\\n")
  for (nm in names(counts)) {
    pct <- round(counts[nm] / length(labels) * 100)
    cat(sprintf("  %-4s : %d (%d%%)\\n", nm, counts[nm], pct))
  }
  invisible(counts)
}
'

MAIN_CODE <- 'raw    <- c(0.1, 2.3, 1.5, 0.8, 3.2, 1.1, 2.7, 0.4)
normed <- normalize(raw)
cat("Normalized:", paste(round(normed, 2), collapse = ", "), "\\n")
labels <- classify(normed)
cat("Labels:", paste(labels, collapse = ", "), "\\n")
tally(labels)
'

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("shinyStep — test app"),

  fluidRow(
    # Left: main program panel
    column(3,
      wellPanel(
        h4("Main program"),
        tags$p(style = "font-size:12px; color:#666;",
          "Functions are chained: normalize → classify → tally.",
          "Toggle 'Debug' on each tab to enable or disable step-through, then click Run.",
          "Use the console inside each debugger to inspect local variables."),
        textAreaInput("main_code", label = NULL,
                      value = MAIN_CODE,
                      rows  = 8, width = "100%"),
        actionButton("run_btn", "Run program",
                     icon  = icon("play"),
                     class = "btn-primary btn-block"),
        tags$hr(),
        tags$p(style = "font-size:11px; color:#888;",
          tags$b("Tips:"), tags$br(),
          "• Next — step one expression", tags$br(),
          "• Step Out — exit current loop/if block", tags$br(),
          "• Continue — run to next pause", tags$br(),
          "• Console — type any R expression, Enter to eval, ↑↓ for history")
      )
    ),

    # Right: one tab per debuggable function
    column(9,
      tabsetPanel(id = "fn_tabs",
        tabPanel("normalize",
          br(),
          stepUI("norm", label = "normalize",
                 height       = "480px",
                 default_code = INIT_NORMALIZE)
        ),
        tabPanel("classify",
          br(),
          stepUI("cls", label = "classify",
                 height       = "480px",
                 default_code = INIT_CLASSIFY)
        ),
        tabPanel("tally",
          br(),
          stepUI("tal", label = "tally",
                 height       = "480px",
                 default_code = INIT_TALLY)
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  runner  <- make_runner()
  run_log <- reactiveVal("")

  norm_mod <- stepServer("norm", fn_name = "normalize",
                         runner = runner, run_log = run_log,
                         initial_code = INIT_NORMALIZE)
  cls_mod  <- stepServer("cls",  fn_name = "classify",
                         runner = runner, run_log = run_log,
                         initial_code = INIT_CLASSIFY)
  tal_mod  <- stepServer("tal",  fn_name = "tally",
                         runner = runner, run_log = run_log,
                         initial_code = INIT_TALLY)

  # Auto-switch to the tab that owns the current pause
  observe({
    req(isTRUE(runner$state$paused))
    owner <- runner$state$pause_owner
    tab <- switch(owner,
      normalize = "normalize",
      classify  = "classify",
      tally     = "tally",
      NULL
    )
    if (!is.null(tab)) updateTabsetPanel(session, "fn_tabs", selected = tab)
  })

  observeEvent(input$run_btn, {
    targets <- c(
      if (norm_mod$enabled()) "normalize",
      if (cls_mod$enabled())  "classify",
      if (tal_mod$enabled())  "tally"
    )
    run_program(
      runner        = runner,
      main_code     = input$main_code,
      debug_targets = targets,
      run_log       = run_log
    )
  })
}

shinyApp(ui, server)
