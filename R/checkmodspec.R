#' Specify and inspect parametric model specification
#'
#' Specify a parametric model (which may represent the analysis or imputation
#' model). Optionally, if a dataset is supplied, explore whether the observed
#' relationships in the specified dataset are consistent with the proposed
#' parametric model.
#'
#' @param formula A symbolic description of the model to be fitted, with the
#'   dependent variable on the left of a ~ operator, and the covariates,
#'   separated by + operators, on the right, specified as a string
#' @param family A description of the error distribution and link function to be
#'   used in the model, specified as a string; family functions that are
#'   supported are "gaussian(identity)" and "binomial(logit)"
#' @param data Optionally, a data frame containing all the variables stated in
#'   the formula
#' @param plot If TRUE (the default), and a dataset is supplied, displays a plot
#'   which can be used to explore the functional form of each covariate in the
#'   specified model if there is evidence of model mis-specification; use plot =
#'   FALSE to disable the plot
#' @param message If TRUE (the default), and a dataset is supplied, displays a
#'   message indicating whether the relationships between the dependent variable
#'   and covariates are likely to be correctly specified or not; use message =
#'   FALSE to suppress the message
#'
#' @return An object of type 'mimod' (a list containing the specified formula,
#'   family, and, if specified, dataset name). Optionally, if required and a
#'   dataset is supplied, a message indicating whether the relationships between
#'   the dependent variable and covariates are likely to be correctly specified
#'   or not. If there is evidence of model mis-specification, optionally returns
#'   a plot of the model residuals versus the fitted values which can be used to
#'   explore the appropriate functional form for the specified model.
#'
#' @export
#'
#' @references Curnow E, Carpenter JR, Heron JE, et al. 2023. Multiple
#'   imputation of missing data under missing at random: compatible imputation
#'   models are not sufficient to avoid bias if they are mis-specified. J Clin
#'   Epidemiol. <doi:10.1016/j.jclinepi.2023.06.011>
#'
#' @examples
#' # Example (incorrectly) assuming a linear relationship
#' checkModSpec(formula="bmi7~matage+mated+pregsize",
#'              family="gaussian(identity)", data=bmi)
#'   ## For the example above, (correctly) assuming a quadratic relationship
#' checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
#'              family="gaussian(identity)", data=bmi)
checkModSpec <- function(formula, family, data=NULL, plot=TRUE, message=TRUE) {

  if (!is.null(data)) {
    if(family == "gaussian(identity)"){
      mod <- stats::glm(stats::as.formula(formula), data=data)
      modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
      modfittest <- mfp2::mfp2(r ~ fitvals, data=modfit, verbose = FALSE)
      pval <- round(1-stats::pchisq(modfittest$null.deviance-modfittest$deviance, modfittest$df.null-modfittest$df.residual),6)

      result1 <- paste("Model mis-specification method: regression of model residuals on a fractional polynomial of the fitted values \n \nP-value:",
                       paste0(pval),collapse = "\n")
        }

    else if(family == "binomial(logit)"){
      mod <- stats::glm(formula=stats::as.formula(formula),family="binomial", data=data)
      modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
      modfittest <- blorr::blr_linktest(mod)
      pval <- round(stats::coef(modfittest)[3,4],6)

      result1 <- paste("Model mis-specification method: Pregibon's link test\n \nP-value:",
                       paste0(pval),collapse = "\n")
      }

    if(pval > 0.1){
      result2 <- paste("\nA large p-value means there is little evidence of model mis-specification. Note that the observed relationships may be distorted by data missing not at random.", collapse = "\n")
    } else {
      result2 <- paste("\nA small p-value means the model may be mis-specified. Check the specification of each relationship in your model, noting that the observed relationships may be distorted by data missing not at random.", collapse = "\n")
    }
    result <- paste(result1, "\n", result2, collapse = "\n")

    #Return message with model check results
    if(message) {message(paste(strwrap(result),collapse="\n"))}

    #Optionally create a plot of residual vs fitted values when evidence of mis-specification
    if (plot & pval <= 0.1){
      if (family == "gaussian(identity)"){
        plot(modfit$fitvals,modfit$r,xlab="",ylab="",
             main="Residuals versus fitted values",
             sub=list("This plot may suggest the appropriate functional form \nfor the specified model",cex=0.8))
      }
      else if (family == "binomial(logit)"){
      arm::binnedplot(modfit$fitvals,modfit$r,xlab="",ylab="",col.int="white",
                      main="Residuals versus (binned) fitted values",
                      sub=list("This plot may suggest the appropriate functional form \nfor the specified model", cex=0.8))
      }
    }
    mimod <- list(formula = formula,family = family,
                  datalab=deparse(substitute(data)))
  } else {

    #Return message with stated formula and reminder about data check
    if(message) {message(paste("The proposed parametric model is:",
                               sQuote(formula),
                               "\n\nNow specify a dataset to explore whether observed relationships in the dataset are consistent with the proposed model",
                               prefix="\n", collapse="\n"))}
    mimod <- list(formula = formula,family = family)
  }
  #Return an object with formula and family
  invisible(mimod)
}


