library(shiny)
library(shinyStep)

ui <- fluidPage(

  tags$script(HTML(
    "Shiny.addCustomMessageHandler('toggle_run_btn',function(msg){",
    "  var el=document.getElementById('run_btn');",
    "  if(el){ el.disabled=msg.disabled;",
    "    el.style.opacity=msg.disabled?'0.45':'';",
    "    el.style.cursor=msg.disabled?'not-allowed':'';",
    "  }",
    "});"
  )),

  fluidRow(

    # ── Left: main script + run ──────────────────────────────────────────────
    column(3,
      wellPanel(
        h4("Main script"),
        fileInput("upload_script", label = NULL,
                  accept      = ".R",
                  buttonLabel = tagList(icon("upload"), " Upload .R"),
                  placeholder = "No file selected"),
        textAreaInput("main_code", label = NULL, value = "",
                      rows = 12, width = "100%",
                      placeholder = "Upload a script or type main code here..."),
        actionButton("run_btn", "Run Program",
                     icon  = icon("play"),
                     class = "btn-primary btn-block")
      )
    ),

    # ── Right: hidden tabset ─────────────────────────────────────────────────
    column(9,
      tabsetPanel(id = "view", type = "hidden",

        # ── Main view ────────────────────────────────────────────────────────
        tabPanel("main",
          br(),
          wellPanel(
            h4("Milestone actions"),
            fluidRow(
              column(5,
                textInput("act_fn_name", label = "Action 1 function name",
                          value = "action1", placeholder = "e.g. action1")),
              column(3, br(),
                actionButton("edit_act_btn", "Edit Action 1",
                             icon = icon("code"), class = "btn-sm btn-default",
                             style = "margin-top:6px;"))
            ),
            fluidRow(
              column(5,
                textInput("act2_fn_name", label = "Action 2 function name",
                          value = "action2", placeholder = "e.g. action2")),
              column(3, br(),
                actionButton("edit_act2_btn", "Edit Action 2",
                             icon = icon("code"), class = "btn-sm btn-default",
                             style = "margin-top:6px;"))
            )
          ),
          wellPanel(
            h4("Regular function"),
            fluidRow(
              column(5,
                textInput("fn_fn_name", label = "Function name",
                          value = "my_fn", placeholder = "e.g. my_fn")),
              column(3, br(),
                actionButton("edit_fn_btn", "Edit Function",
                             icon = icon("code"), class = "btn-sm btn-default",
                             style = "margin-top:6px;"))
            )
          )
        ),

        # ── Action 1 editor ──────────────────────────────────────────────────
        tabPanel("action",
          br(),
          stepUI("act", label = "action1", height = "500px")
        ),

        # ── Action 2 editor ──────────────────────────────────────────────────
        tabPanel("action2",
          br(),
          stepUI("act2", label = "action2", height = "500px")
        ),

        # ── Regular function editor ───────────────────────────────────────────
        tabPanel("fn",
          br(),
          stepUI("fn", label = "my_fn", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  runner  <- make_runner()
  run_log <- reactiveVal("")

  # ── Action 1 ───────────────────────────────────────────────────────────────
  act_saved_rv <- reactiveVal("")
  act_restore  <- reactiveVal(0L)
  act_initial  <- reactive({ act_restore(); act_saved_rv() })

  act_mod <- stepServer("act", fn_name = "action",
                        runner = runner, run_log = run_log,
                        initial_code = act_initial)

  # ── Action 2 ───────────────────────────────────────────────────────────────
  act2_saved_rv <- reactiveVal("")
  act2_restore  <- reactiveVal(0L)
  act2_initial  <- reactive({ act2_restore(); act2_saved_rv() })

  act2_mod <- stepServer("act2", fn_name = "action2",
                         runner = runner, run_log = run_log,
                         initial_code = act2_initial)

  # ── Regular function ────────────────────────────────────────────────────────
  fn_saved_rv <- reactiveVal("")
  fn_restore  <- reactiveVal(0L)
  fn_initial  <- reactive({ fn_restore(); fn_saved_rv() })

  fn_mod <- stepServer("fn", fn_name = "fn",
                       runner = runner, run_log = run_log,
                       initial_code = fn_initial)

  # ── Grey out Run button while program is running or paused ─────────────────
  observe({
    running <- isTRUE(runner$state$running) || isTRUE(runner$state$paused)
    session$sendCustomMessage("toggle_run_btn", list(disabled = running))
  })

  # ── Navigation ─────────────────────────────────────────────────────────────
  observeEvent(input$edit_act_btn,  { updateTabsetPanel(session, "view", selected = "action")  })
  observeEvent(input$edit_act2_btn, { updateTabsetPanel(session, "view", selected = "action2") })
  observeEvent(input$edit_fn_btn,   { updateTabsetPanel(session, "view", selected = "fn")      })

  observeEvent(act_mod$save_clicked(),  { act_saved_rv(act_mod$get_code())   }, ignoreInit = TRUE)
  observeEvent(act2_mod$save_clicked(), { act2_saved_rv(act2_mod$get_code()) }, ignoreInit = TRUE)
  observeEvent(fn_mod$save_clicked(),   { fn_saved_rv(fn_mod$get_code())     }, ignoreInit = TRUE)

  observeEvent(act_mod$back_clicked(), {
    act_restore(act_restore() + 1L)
    updateTabsetPanel(session, "view", selected = "main")
  }, ignoreInit = TRUE)

  observeEvent(act2_mod$back_clicked(), {
    act2_restore(act2_restore() + 1L)
    updateTabsetPanel(session, "view", selected = "main")
  }, ignoreInit = TRUE)

  observeEvent(fn_mod$back_clicked(), {
    fn_restore(fn_restore() + 1L)
    updateTabsetPanel(session, "view", selected = "main")
  }, ignoreInit = TRUE)

  # ── Auto-switch to the editor for whichever function is paused ─────────────
  observe({
    req(isTRUE(runner$state$paused), identical(runner$state$pause_owner, "action"))
    updateTabsetPanel(session, "view", selected = "action")
  })

  observe({
    req(isTRUE(runner$state$paused), identical(runner$state$pause_owner, "action2"))
    updateTabsetPanel(session, "view", selected = "action2")
  })

  observe({
    req(isTRUE(runner$state$paused), identical(runner$state$pause_owner, "fn"))
    updateTabsetPanel(session, "view", selected = "fn")
  })

  # ── Upload ─────────────────────────────────────────────────────────────────
  observeEvent(input$upload_script, {
    req(input$upload_script)
    txt <- tryCatch(
      paste(readLines(input$upload_script$datapath, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    if (!is.null(txt))
      updateTextAreaInput(session, "main_code", value = txt)
  })

  # ── Run ────────────────────────────────────────────────────────────────────
  observeEvent(input$run_btn, {
    act_name  <- trimws(input$act_fn_name)  %||% "action1"
    act2_name <- trimws(input$act2_fn_name) %||% "action2"
    fn_name   <- trimws(input$fn_fn_name)   %||% "my_fn"

    main_code <- input$main_code
    if (!identical(act_name,  "action")  && nzchar(act_name))
      main_code <- paste0(act_name,  " <- action\n",  main_code)
    if (!identical(act2_name, "action2") && nzchar(act2_name))
      main_code <- paste0(act2_name, " <- action2\n", main_code)
    if (!identical(fn_name,   "fn")      && nzchar(fn_name))
      main_code <- paste0(fn_name,   " <- fn\n",      main_code)

    debug_targets <- c(
      if (act_mod$enabled())  "action"  else character(0),
      if (act2_mod$enabled()) "action2" else character(0),
      if (fn_mod$enabled())   "fn"      else character(0)
    )

    run_program(
      runner        = runner,
      main_code     = main_code,
      debug_targets = debug_targets,
      run_log       = run_log
    )
  })
}

shinyApp(ui, server)
