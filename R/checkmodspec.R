#' Check specification of the proposed model (analysis or imputation) using the observed data
#'
#' @param formula a formula object: a symbolic description of the model to be fitted, with the model outcome on the left of a ~ operator, and the covariates, separated by + operators, on the right.
#' @param family a description of the error distribution and link function to be used in the model. Family functions that are supported are gaussian(identity) and binomial(logit).
#' @param data a data frame containing all the variables stated in the formula.
#'
#' @importFrom data.table setDT
#' @return
#' @export
#'
#' @examples
checkmodspec <- function(formula, family, data) {
  if (is.character(family))
    family <- get(family, mode = "function")
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  mod <- stats::glm(formula, family=family, data=data)
  fam <- deparse(substitute(family))
  if(fam == "gaussian(identity)"){
    modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
    modfittest <- mfp::mfp(r ~ fp(fitvals, df = 4, select=0.05), data=modfit)
    pval <- 1-stats::pchisq(modfittest$null.deviance-modfittest$deviance, modfittest$df.null-modfittest$df.residual)
    cat("Model mis-specification test (regression of model residuals on a fractional polynomial of the fitted values)",
        "P-value:",
        pval,
        fill=TRUE)
    if(pval > 0.1){
      cat("A large p-value means there is little evidence of model mis-specification.")
    } else {
      cat("A small p-value means the model may be mis-specified.
Check the specification of each relationship in your model.")
    }
  }
  else if(fam == "binomial(logit)"){
    modfittest <- blorr::blr_linktest(mod)
    pval <- stats::coef(modfittest)[3,4]
    cat("Model mis-specification test (Pregibon's link test)",
        "P-value:",
        pval,
        fill=TRUE)
    if(pval > 0.1){
      cat("\033[0;32mA large p-value means there is little evidence of model mis-specification.\033[0m")
    } else {
      cat("\033[0;31mA small p-value means the model may be mis-specified.
Check the specification of each relationship in your model.\033[0m")
    }
  }
}
