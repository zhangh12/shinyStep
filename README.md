# shinyStep

Step through R functions expression-by-expression inside a Shiny app — like RStudio's debugger, but in the browser.

## Features

- **Next / Step Out / Continue / Stop** buttons with green-arrow line highlight in the editor
- Handles `for`, `while`, `repeat`, `if` / `else if` / `else`, and early `return()`
- In-frame console: evaluate any R expression in the paused function's local environment
- Per-function **Debug** toggle — uncheck to run a function at full speed without stepping, even mid-run
- Works with functions called directly from main code **and** functions invoked inside synchronous wrappers (e.g. a simulation engine's `controller$run()`)
- Multiple functions can be registered and debugged in sequence within a single run

## Installation

```r
remotes::install_github("zhangh12/shinyStep")
```

Requires `shiny >= 1.7.0` and `shinyAce >= 0.4.0`.

## Usage

Place one `stepUI()` per function in the UI and wire up `stepServer()` in the server. A single `run_program()` call executes the main script and pauses at registered functions.

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

## fn_name and alias injection

`fn_name` in `stepServer()` is the **internal proxy name** — the name under which shinyStep installs its intercepting closure in the execution environment. It does not have to match what the user types in the editor, but it must match `debug_targets` in `run_program()` and is what appears in the status badge.

When the user's function has a different name in `main_code` (e.g. `action1` in a script but `fn_name = "action"` internally), inject an alias at the top of `main_code` before calling `run_program()`:

```r
main_code <- paste0("action1 <- action\n", main_code)
```

This causes `action1` in the script to resolve to the proxy named `"action"`, keeping everything in sync.

## Nested calls

shinyStep works even when the target function is called from inside a long-running synchronous wrapper (e.g. a simulation engine). When the proxy fires it throws a typed condition that unwinds the enclosing call, returning control to the Shiny event loop. On Continue, the wrapper is re-executed from the start with the just-debugged function in a skip list so it passes through silently — allowing the next registered function to pause normally.

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

Wire up in the server. `fn_name` is the internal proxy name — it must match the corresponding entry in `debug_targets` passed to `run_program()`.

Returns:

- `save_clicked` — reactive, fires when Save is clicked (stays in editor)
- `back_clicked` — reactive, fires when Back is clicked (discards unsaved changes)
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

## Test app

A working example with two milestone action functions and one standalone function is in `test_app/`:

```r
shiny::runApp("test_app")
```
