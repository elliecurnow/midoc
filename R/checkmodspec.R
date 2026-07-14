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
#' @param by Optional stratification variable(s), specified as a string (space
#'   delimited) or a list of factors; if specified, the parametric model will be
#'   fit for each subset of the data determined by the values of the factor(s)
#' @param data Optionally, a data frame containing all the variables stated in
#'   the formula and if specified, stratification variable(s)
#' @param plot If TRUE (the default), and a dataset is supplied, displays a plot
#'   which can be used to explore the form of the specified model; note that
#'   stratification variables are ignored in the plot; use plot = FALSE to
#'   disable the plot
#' @param message If TRUE (the default), and a dataset is supplied, displays a
#'   message indicating whether the relationships between the dependent variable
#'   and covariates are likely to be correctly specified or not; use message =
#'   FALSE to suppress the message
#'
#' @return An object of type 'mimod' (a list containing the specified formula,
#'   family, and, if specified, dataset name). Optionally, if required and a
#'   dataset is supplied, a message indicating whether the relationships between
#'   the dependent variable and covariates are likely to be correctly specified
#'   or not.
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
checkModSpec <- function(formula, family, by=NULL, data=NULL, plot=TRUE, message=TRUE) {

  #Check the specified family is supported
  if (length(family) != 1 || !family %in% c("gaussian(identity)", "binomial(logit)")) {
    stop("\n\n'family' must be either \"gaussian(identity)\" or \"binomial(logit)\"\n\n",
         call.=TRUE)
  }

  if (!is.null(data)) {
    if(family == "gaussian(identity)"){

      #Create modfit object for plotting purposes
      mod <- stats::glm(stats::as.formula(formula), data=data)
      modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])

      if(is.null(by)){
        modfitres <- mfp2::mfp2(r ~ fitvals, data=modfit, verbose = FALSE)
        modfittest <- summary(modfitres)
      } else {
        bylist <- unlist(strsplit(by," "))
        modfittest <- by(data, data[,c(bylist)],
                    function(x) {
                      mod <- stats::glm(stats::as.formula(formula), data = x)
                      modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
                      modfitres <- mfp2::mfp2(r ~ fitvals, data=modfit, verbose = FALSE)
                      summary(modfitres)
                      })
        names(dimnames(modfittest)) = bylist
      }
      #pval <- round(1-stats::pchisq(modfittest$null.deviance-modfittest$deviance, modfittest$df.null-modfittest$df.residual),6)

      result1 <- paste("Method used to explore model specification: regression of model residuals (y) on a fractional
                       polynomial of the fitted values (fitvals). If stratification variable(s) are specified,
                       results are subsetted by the values of the factor(s).\n",
                       #paste0(pval),
                       "\n\n", paste0(gsub(" ", "@",utils::capture.output(modfittest)),"\n",collapse = "\n"),
                       collapse = "\n")
        }

    else if(family == "binomial(logit)"){
      #Create modfit object for plotting purposes
      mod <- stats::glm(formula=stats::as.formula(formula),family="binomial", data=data)
      modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])

      if(is.null(by)){
        modfittest <- blorr::blr_linktest(mod)
        #pval <- round(stats::coef(modfittest)[3,4],6)
      } else {
        bylist <- unlist(strsplit(by," "))
        modfittest <- by(data, data[,c(bylist)],
                         function(x) {
                           mod <- stats::glm(stats::as.formula(formula),family="binomial", data = x)
                           modfit <- data.frame(r=mod[["residuals"]],fitvals=mod[["fitted.values"]])
                           blorr::blr_linktest(mod)
                         })
        names(dimnames(modfittest)) = bylist
      }
      result1 <- paste("Method used to explore model specification:
                       Pregibon's link test, a regression of the model outcome (resp)
                       on the fitted values (fit) and the square of the fitted values (fit2).
                       If stratification variable(s) are specified,
                       results are subsetted by the values of the factor(s).\n",
                       #paste0(pval)
                       "\n\n", paste0(gsub(" ", "@",utils::capture.output(modfittest)),"\n",collapse = "\n"),
                       collapse = "\n")
      }

    #Removed condition on pval
    #if(pval > 0.1){
    #} else {
    #result2 <- paste("\nA strong relationship between the model residuals and fitted values means the model may be mis-specified. Check the specification of each relationship in your model, noting that the observed relationships may be distorted by data missing not at random.", collapse = "\n")
    #}
    if(family == "gaussian(identity)"){
      result2 <- paste("\nInterpretation: A weak relationship between the model residuals and fitted values
                       means there is little evidence of model mis-specification. A strong relationship
                       between the model residuals and fitted values means the model may be mis-specified.
                       \n\nNote that an intercept-only model will be displayed if there is a weak relationship between
                       the model residuals and fitted values.
                       \n\nConsider whether the specified model is plausible for your study, and update it accordingly.
                       Note that the observed relationships may be distorted by data missing not at random.", collapse = "\n")

    } else {
      result2 <- paste("\nInterpretation: A weak relationship between the model outcome and
                       the square of the fitted values (fit2)
                       means there is little evidence of model mis-specification. A strong relationship
                       between the model outcome and
                       the square of the fitted values (fit2) means the model may be mis-specified.
                       \n\nConsider whether the specified model is plausible for your study, and update it accordingly.
                       Note that the observed relationships may be distorted by data missing not at random.", collapse = "\n")

    }
    result <- paste(result1, "\n", result2, collapse = "\n")

    #Return message with model check results
    if(message) {message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))}

    #Optionally create a plot of residual vs fitted values
    #if (plot & pval <= 0.1){
    if (plot){
      if (family == "gaussian(identity)"){
        plot(x=modfit$fitvals,y=modfit$r,xlab="",ylab="Residuals",
             main="Residuals versus fitted values \nbased on all data (not stratified)",
             sub=list("This plot may suggest the appropriate functional form \nfor the specified model",cex=0.8))
        graphics::title(xlab="Fitted values", mgp=c(2,1,0))
      }
      else if (family == "binomial(logit)"){
        arm::binnedplot(x=modfit$fitvals,y=modfit$r,xlab="",ylab="Residuals",col.int="white",
                      main="Residuals versus (binned) fitted values \nbased on all data (not stratified)",
                      sub=list("This plot may suggest the appropriate functional form \nfor the specified model", cex=0.8))
        graphics::title(xlab="Fitted values", mgp=c(2,1,0))
      }
    }
    mimod <- list(formula = formula,family = family,by = by,
                  datalab=deparse(substitute(data)))
  } else {

    #Return message with stated formula and reminder about data check
    if(message) {
      if(is.null(by)){
        message(paste("The proposed parametric model is:",
                               sQuote(formula),
                               "\n\nNow specify a dataset to explore whether observed relationships in the dataset are consistent with the proposed model",
                               "\n", collapse="\n"))
        } else {
          message(paste("The proposed parametric model is:",
                        sQuote(formula),
                        "\n\nstratified by:",
                        paste(by),
                        "\n\nNow specify a dataset to explore whether observed relationships in the dataset are consistent with the proposed model",
                        "\n", collapse="\n"))}
        }
    mimod <- list(formula = formula,family = family,by=by)
  }
  #Return an object with formula and family
  invisible(mimod)
}


