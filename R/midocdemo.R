#' @export
midocdemo <- function(dir = getwd()) {
  appDir <- system.file("shiny-examples/midocdemo.rmd", package = "midoc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `midoc`.", call. = FALSE)
  }

  rmarkdown::render(input = appDir,
                    output_file = "midocdemo.html",
                    output_dir = dir,
                    clean = TRUE)
}
