#' Fractional Polynomial Transformation as per {mfp} package: Ambler G, Benner A (2022). mfp: Multivariable Fractional Polynomials. R package version 1.5.2.2, <https://CRAN.R-project.org/package=mfp>.
#'
#' @param x input variable
#' @param df degrees of freedom. df = 4 corresponds to an FP model with maximum of 2 permitted degrees of freedom
#' @param select sets the variable selection level
#' @param alpha sets the FP selection level
#' @param scale use pre-transformation scaling
#'
#' @keywords internal
#'
fp <- function (x, df = 4, select = NA, alpha = NA, scale = TRUE)
{
  name <- deparse(substitute(x))
  attr(x, "df") <- df
  attr(x, "alpha") <- alpha
  attr(x, "select") <- select
  attr(x, "scale") <- scale
  attr(x, "name") <- name
  x
}
