#' Launch My Shiny App
#'
#' This function starts the Shiny app included in the package.
#'
#' @export
launchMyApp <- function() {
  appDir <- system.file("shinyapp", package = "bis620.2023")
  if (appDir == "") {
    stop("Shiny app not found in the package")
  }
  shiny::runApp(appDir)
}

