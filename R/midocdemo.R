#' Run a Shiny demo of midoc
#' @export
midocdemo <- function() {
  #appDir <- system.file("vignettes/midocdemoShiny.rmd", package = "midoc")
  #if (appDir == "") {
  #  stop("Could not find example directory. Try re-installing `midoc`.", call. = FALSE)
  #}

  rmarkdown::run("vignettes/midocdemoShiny.Rmd")
}
