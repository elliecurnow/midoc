#' Inspect parametric model specification
#'
#' Explore whether the observed relationships in the specified dataset are
#' consistent with the proposed parametric model (which may represent the
#' analysis or imputation model).
#'
#' @param formula A symbolic description of the model to be fitted, with the
#'   dependent variable on the left of a ~ operator, and the covariates,
#'   separated by + operators, on the right, specified as a string
#' @param family A description of the error distribution and link function to be
#'   used in the model, specified as a string; family functions that are
#'   supported are "gaussian(identity)" and "binomial(logit)"
#' @param data A data frame containing all the variables stated in the formula
#' @param message If TRUE (the default), displays a message indicating whether the
#'   relationships between the dependent variable and covariates are likely to
#'   be correctly specified or not; use message = FALSE to suppress the message
#' @param plot If TRUE (the default) and there is evidence of model mis-specification,
#'   displays a plot which can be used to explore the functional form of each
#'   covariate in the specified model; use plot = FALSE to disable the plot
#'
#' @return An object of type 'miprop' (a list containing the
#'   specified formula, family, and dataset name), plus, optionally, messages
#'   and plots indicating whether the relationships between the dependent
#'   variable and covariates are likely to be correctly specified or not
#'
#' @export
#'
#' @examples
#' # Example (incorrectly) assuming a linear relationship
#' checkModSpec(formula="bmi7~matage+mated+pregsize",
#'   family="gaussian(identity)", data=bmi)
#'   ## For the example above, (correctly) assuming a quadratic relationship
#' checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
#'   family="gaussian(identity)", data=bmi)
checkModSpec <- function(formula, family, data, message=TRUE, plot=TRUE) {

  if(family == "gaussian(identity)"){
    mod <- stats::glm(stats::as.formula(formula), data=data)
    modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
    modfittest <- mfp::mfp(r ~ fp(fitvals, df = 4, select=0.05), data=modfit)
    pval <- 1-stats::pchisq(modfittest$null.deviance-modfittest$deviance, modfittest$df.null-modfittest$df.residual)

    if (message){
      cat(strwrap("Model mis-specification method: regression of model residuals
on a fractional polynomial of the fitted values"),
          "\n",
          "P-value:",
          pval,"\n",fill=TRUE)
      if(pval > 0.1){
          cat(strwrap("A large p-value means there is little evidence of model mis-specification."),"\n",fill=TRUE)
      } else {
          cat(strwrap("A small p-value means the model may be mis-specified. Check
the specification of each relationship in your model."),"\n",fill=TRUE)

      #Optionally create a plot of residual vs fitted values
      if (plot){
          plot(modfit$fitvals,modfit$r,xlab="",ylab="",
               main="Residuals versus fitted values",
               sub=list("This plot may suggest the appropriate functional form \nfor the specified model",cex=0.8))
        }
      }
    }
  }

  else if(family == "binomial(logit)"){
    mod <- stats::glm(formula=stats::as.formula(formula),family="binomial", data=data)
    modfittest <- blorr::blr_linktest(mod)
    pval <- stats::coef(modfittest)[3,4]
    if (message){
      cat(strwrap("Model mis-specification method: Pregibon's link test"),
          "\n",
          "P-value:",
          pval,"\n", fill=TRUE)
      if(pval > 0.1){
          cat(strwrap("A large p-value means there is little evidence of model mis-specification."),"\n", fill=TRUE)
      } else {
          cat(strwrap("A small p-value means the model may be mis-specified. Check the
specification of each relationship in your model."),"\n",fill=TRUE)

        #Optionally create a plot of deviance residual vs binned fitted values
        modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
        if (plot){
          arm::binnedplot(modfit$fitvals,modfit$r,xlab="",ylab="",col.int="white",
                      main="Residuals versus (binned) fitted values",
                      sub=list("This plot may suggest the appropriate functional form \nfor the specified model", cex=0.8))
        }
      }
    }
  }

#Output an object with formula and family
mimod <- list(formula = formula,family = family,
              datalab=deparse(substitute(data)))
invisible(mimod)
}

## Fractional Polynomial Transformation as per {mfp} package: Ambler G, Benner A (2022). mfp: Multivariable Fractional Polynomials. R package version 1.5.2.2, <https://CRAN.R-project.org/package=mfp>.
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
