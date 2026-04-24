#' Launch the bundled demo Shiny app
#'
#' Runs the demo application shipped under \code{inst/test_app/}, which shows
#' both solo and embedded modules with a shared packages prelude and a host
#' \code{Main program} panel.
#'
#' @param ... Further arguments passed to \code{\link[shiny]{runApp}}, e.g.
#'   \code{port}, \code{host}, \code{launch.browser}.
#' @return This function is invoked for its side effect and does not return.
#' @examples
#' \dontrun{
#'   shinyStep::run_demo()
#' }
#' @export
run_demo <- function(...) {
  app_dir <- system.file("test_app", package = "shinyStep")
  if (!nzchar(app_dir))
    stop("Demo app not found. Reinstall shinyStep to include inst/test_app.",
         call. = FALSE)
  shiny::runApp(app_dir, ...)
}
