#' Run an interactive vignette for the midoc package
#'
#' Runs an interactive version of the midoc vignette: Multiple Imputation DOCtor
#' (midoc). In the interactive version, you can apply midoc functions using your
#' own DAG and data.
#'
#' @export
#'
midocdemo <- function() {
  appDir <- system.file("doc/midocdemoShiny.rmd", package = "midoc", mustWork=TRUE)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `midoc`.", call. = FALSE)
  }

  rmarkdown::run(appDir)
}
