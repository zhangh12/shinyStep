# shinyStep

Step through R functions expression-by-expression inside a Shiny app — like RStudio's debugger, but in the browser.

## Features

- **Next / Step Out / Continue / Stop** buttons with green-arrow line highlight in the editor
- Handles `for`, `while`, `repeat`, `if` / `else if` / `else`, and early `return()`
- In-frame console: evaluate any R expression in the paused function's local environment
- Per-function **Debug** toggle — uncheck to run a function at full speed without stepping, even mid-run
- Return values and default function parameters handled correctly

## Installation

```r
remotes::install_github("zhangh12/shinyStep")
```

Requires `shiny >= 1.7.0` and `shinyAce >= 0.4.0`.

## Usage

Define your function code as a string, place one `stepUI()` per function in the UI, and wire up `stepServer()` in the server. A single `run_program()` call executes the main script and pauses at registered functions.

```r
library(shiny)
library(shinyStep)

fn_code <- '
my_fn <- function(x) {
  total <- 0
  for (v in x) {
    total <- total + v
  }
  total
}
'

ui <- fluidPage(
  actionButton("run", "Run", class = "btn-primary"),
  stepUI("my_fn", label = "my_fn", default_code = fn_code)
)

server <- function(input, output, session) {
  runner  <- make_runner()
  run_log <- reactiveVal("")

  mod <- stepServer("my_fn", fn_name = "my_fn",
                    runner = runner, run_log = run_log,
                    initial_code = fn_code)

  observeEvent(input$run, {
    run_program(
      runner        = runner,
      main_code     = "result <- my_fn(c(1, 2, 3, 4, 5))",
      debug_targets = if (mod$enabled()) "my_fn" else character(0),
      run_log       = run_log
    )
  })
}

shinyApp(ui, server)
```

Click **Run**, then use **Next** to step through each expression. Type `total` in the console and press **Enter** to inspect the running sum.

## Console keyboard shortcuts

| Key          | Action                  |
|:-------------|:------------------------|
| `Enter`      | Submit expression       |
| `↑` / `↓`   | Navigate input history  |
| `Ctrl+L`     | Clear output            |

Selected text is automatically copied to the clipboard.

## API

### `make_runner()`

Call once inside `server()`. Returns the runner object shared by all modules and `run_program()`.

### `stepUI(id, label, height, theme, default_code)`

Place in the UI for each function to debug. The toolbar includes a **Debug** checkbox — uncheck it to skip stepping for that function without changing any code.

### `stepServer(id, fn_name, runner, run_log, initial_code)`

Wire up in the server. `fn_name` must match the function name as it appears in the editor code.

Returns:

- `back_clicked` — reactive, fires when the Back button is clicked
- `fn_name` — registered function name (character)
- `get_code` — function, returns current editor text
- `enabled` — reactive, `TRUE` when the Debug checkbox is ticked

### `run_program(runner, main_code, debug_targets, run_log)`

Execute `main_code` (a character string of R expressions). Functions named in `debug_targets` pause for step-through; others run silently. The **Debug** checkbox in each module also updates `debug_targets` live during a run.

### Step-control functions

These are called internally by `stepServer` but are exported for advanced use.

| Function                                  | Description             |
|:------------------------------------------|:------------------------|
| `step_fn(runner, run_log)`                | Step one expression     |
| `step_out_frame(runner, run_log)`         | Exit the current block  |
| `continue_to_next_pause(runner, run_log)` | Run to the next pause   |
| `stop_runner(runner, run_log)`            | Abort execution         |

## Example

A complete three-function pipeline (`normalize → classify → tally`) is in `test_app/`:

```r
shiny::runApp("test_app")
```
