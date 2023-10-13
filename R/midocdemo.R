#' Run a Shiny demo of midoc
#' @export
midocdemo <- function() {
  appDir <- system.file("doc/midocdemoShiny.rmd", package = "midoc", mustWork=TRUE)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `midoc`.", call. = FALSE)
  }

  rmarkdown::run(appDir)
}
