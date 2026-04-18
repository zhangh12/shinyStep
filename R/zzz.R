.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix   = "shinyStep",
    directoryPath = system.file("www", package = "shinyStep")
  )
}
