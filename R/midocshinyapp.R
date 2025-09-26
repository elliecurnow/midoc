#' Run a browser-based version of the midoc package
#'
#' Runs a browser-based version of the midoc package. In this version, you can
#' explore midoc functions using exemplar datasets or apply midoc functions
#' using your own DAG and data.
#'
#' @return A browser-based version of the midoc package
#' @export
#'
#' @examplesIf interactive()
#' # Run the browser-based version of the midoc package
#' midocShinyApp()
midocShinyApp <- function() {
  appDir <- system.file("shinyexamples/app_local.R", package = "midoc", mustWork=TRUE)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `midoc`.", call. = FALSE)
  }

  shiny::runApp(appDir)
}
