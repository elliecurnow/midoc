#' Performs multiple imputation
#'
#' Performs multiple imputation using \link[mice]{mice}, based on the 'mice'
#' options and dataset specified by a call to  \link[midoc]{proposeMI}.
#'
#' @param mipropobj An object of type 'miprop', created by a call to 'proposeMI'
#' @param seed An integer that is used to set the seed of the 'mice' call
#' @param substmod Optionally, a symbolic description of the substantive model
#'   to be fitted, specified as a string; if supplied, the model will be fitted
#'   to each imputed dataset and the results pooled
#'
#' @return A 'mice' object of type 'mids'
#'
#' @export
#'
#' @examples
#' # First specify the imputation model as a 'mimod' object
#' mimod_bmi7 <- checkModSpec(
#'   formula="bmi7~matage+I(matage^2)+mated+pregsize",
#'   family="gaussian(identity)", data=bmi)
#' # Save the proposed 'mice' options as a 'miprop' object
#' miprop <- proposeMI(mimodobj=mimod_bmi7, data=bmi)
#' # Create the set of imputed datasets using the proposed 'mice' options
#' imp <- doMImice(miprop,123)
#'
#' # Additionally, fit the substantive model to each imputed dataset and display
#'   ## the pooled results
#' doMImice(miprop, 123, substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")
doMImice <- function(mipropobj, seed, substmod = " ") {

  mids <- mice::mice(
        data = mipropobj$data,
        m = mipropobj$m,
        method = mipropobj$method,
        formulas = mipropobj[["formulas"]],
        maxit = 10,
        printFlag = FALSE,
        seed = seed)

  #Optionally return the pooled estimates
  #substexpr <- deparse(substitute(substmod))
  if(substmod != " "){
    cat("Given the substantive model:", substmod, "\n", strwrap("multiple imputation estimates are as follows:"),"\n",fill=TRUE)
    print(summary(mice::pool(with(mids,parse(text=substmod, keep.source=FALSE))),conf.int=TRUE))
  }
  else {
    cat(strwrap("Now you have created your multiply imputed datasets, you can perform your analysis
        and pool the results using the 'mice' functions 'with()' and 'pool()'"),"\n",fill=TRUE)}

  invisible(mids)

}
