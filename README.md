# shinyStep

Step through R functions expression-by-expression inside a Shiny app — like RStudio's debugger, but in the browser.

## Two kinds of function debugging

`shinyStep` is organised around **two orthogonal ways** a function can be debugged:

- **Solo** — the function is tested **in isolation**. You supply argument values in the UI and click **Test**; the package builds `fn_name(arg1 = value1, ...)` behind the scenes, runs it, and pauses at the first body line so you can step through. Nothing outside the function executes.
- **Embedded** — the function is called from inside a **larger program** that the host app writes and launches. When its **Debug** checkbox is ticked, execution pauses at the first body line every time the surrounding program invokes it. The host decides what runs before and after.

One runner can host any mix of solo and embedded modules. Solo modules are **never** debug-paused during an embedded `run_program()` — they exist only to exercise a function standalone via their own Test button.

## Editor conventions

The editor holds the **function body only** — no `fn_name <- function(...) { ... }` wrapper. Name and arguments live in structured UI above the editor:

- A **Function name** text input (defaults to the module id if left blank).
- An **Arguments** card: rows of `name | default | [test value]` with a "+ Add argument" button. (Test value appears in solo mode only.)

If you accidentally paste a full `fn <- function(args) { body }` wrapper into the editor, the package strips it defensively and keeps the inner body.

## Installation

```r
remotes::install_github("zhangh12/shinyStep")
```

Requires `shiny >= 1.7.0` and `shinyAce >= 0.4.0`.

## Solo example

```r
library(shiny)
library(shinyStep)

ui <- fluidPage(
  soloStepUI("sum_vec", label = "sum_vec", default_body =
    "total <- 0\nfor (v in x) {\n  total <- total + v\n}\ntotal")
)

server <- function(input, output, session) {
  runner  <- make_runner()
  run_log <- reactiveVal("")

  soloStepServer("sum_vec", runner, run_log,
    initial_fn_name = "sum_vec",
    initial_args = list(
      list(name = "x", default = "", test_value = "1:5")
    )
  )
}

shinyApp(ui, server)
```

Click **Test**, then **Next** to step through. Type `total` in the terminal and press Enter to inspect the running sum.

## Embedded example

```r
library(shiny)
library(shinyStep)

ui <- fluidPage(
  textAreaInput("main_code", "Main program", rows = 8, width = "100%",
                value = "result <- greet('world')\nprint(result)"),
  actionButton("run", "Run Program", class = "btn-primary"),
  embeddedStepUI("greet", label = "greet", default_body =
    "msg <- paste('Hello,', name)\nmsg")
)

server <- function(input, output, session) {
  runner  <- make_runner()
  run_log <- reactiveVal("")

  embeddedStepServer("greet", runner, run_log,
    initial_fn_name = "greet",
    initial_args = list(list(name = "name", default = "'stranger'"))
  )

  observeEvent(input$run, {
    run_program(runner, main_code = input$main_code, run_log = run_log)
    # debug_targets = NULL → auto-collect from embedded modules whose
    # Debug checkbox is ticked. Equivalent to: debug_targets = "greet".
  })
}

shinyApp(ui, server)
```

## API

### `make_runner()`

Call once inside `server()`. Returns the runner object shared by every module and by `run_program()`.

### Solo module: `soloStepUI(id, label, height, theme, default_body)` + `soloStepServer(id, runner, run_log, initial_fn_name, initial_body, initial_args)`

A solo module tests a single function in isolation. The toolbar includes a **Test** button that reads the test values from the arguments table, assembles `fn_name(arg = value, ...)`, and launches the runner pausing at the first body line.

`soloStepServer` returns a list with:

- `save_clicked`, `back_clicked` — reactives that fire on Save / Back.
- `fn_name` — reactive, current function name.
- `get_fn_name`, `get_body`, `get_args` — functions returning persisted state (useful for host-side auto-save).

### Embedded module: `embeddedStepUI(id, label, height, theme, default_body)` + `embeddedStepServer(id, runner, run_log, initial_fn_name, initial_body, initial_args)`

An embedded module describes a function that the host's `main_code` will call. The toolbar includes a **Debug** checkbox; when ticked, `run_program()` will pause at the first body line whenever the surrounding program invokes the function.

Return value is identical to `soloStepServer`, plus:

- `enabled` — reactive, `TRUE` when Debug is ticked.

### `run_program(runner, main_code, debug_targets = NULL, run_log)`

Execute `main_code` (a character string of R expressions).

- `debug_targets = NULL` (default) — auto-collect every embedded module whose Debug checkbox is currently ticked. Solo modules are never auto-included.
- `debug_targets = character(0)` — run to completion without pausing.
- `debug_targets = c("fn_a", "fn_b")` — pause at these specific names, regardless of checkbox state.

Modules with a blank body are skipped at runtime; if `main_code` references such a function, the user sees a standard "could not find function" error.

### Step-control functions (exported for advanced use)

| Function | Description |
|:-|:-|
| `step_fn(runner, run_log)` | Step one expression (auto-expands compound blocks) |
| `step_out_frame(runner, run_log)` | Exit the current loop / if block |
| `continue_to_next_pause(runner, run_log)` | Run to the next pause or end |
| `stop_runner(runner, run_log)` | Abort execution |

## Features

- Handles `for`, `while`, `repeat`, `if` / `else if` / `else`, early `return()`, `break`, `next` at any nesting depth.
- In-frame console: evaluate any R expression in the paused function's local environment.
- Green-arrow line highlight in the Ace editor, mapped back to the user's body (the `fn <-` header line is hidden from the user).
- Works with functions called directly from `main_code` **and** functions invoked inside synchronous wrappers (e.g. a simulation engine's `controller$run()`). When a proxy pauses it throws a typed condition that unwinds the wrapper; Continue replays the wrapper with the just-debugged function in a skip list.
- Per-embedded-module Debug toggle — works live, mid-run.

## Console keyboard shortcuts

| Key | Action |
|:-|:-|
| `Enter` | Submit expression |
| `↑` / `↓` | Navigate input history |
| `Ctrl+L` | Clear output |

Selected text is automatically copied to the clipboard.

## Test app

A working example with one solo generator and one embedded action is in `test_app/`:

```r
shiny::runApp("test_app")
```
