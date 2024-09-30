#' Performs multiple imputation
#'
#' Creates multiple imputations using \link[mice]{mice}, based on the options
#' and dataset specified by a call to \link[midoc]{proposeMI}. If a substantive
#' model is specified, also calculates the pooled estimates using
#' \link[mice]{pool}.
#'
#' @param mipropobj An object of type 'miprop', created by a call to 'proposeMI'
#' @param seed An integer that is used to set the seed of the 'mice' call
#' @param substmod Optionally, a symbolic description of the substantive model
#'   to be fitted, specified as a string; if supplied, the model will be fitted
#'   to each imputed dataset and the results pooled
#' @param message If TRUE (the default), displays a message summarising the
#'   analysis that has been performed; use message = FALSE to suppress the
#'   message
#'
#' @return A 'mice' object of class 'mids' (the multiply imputed datasets).
#'  Optionally, a message summarising the analysis that has been performed.
#'
#' @export
#'
#' @examples
#' # First specify the imputation model as a 'mimod' object
#' ## (suppressing the message)
#' mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
#'                            family="gaussian(identity)",
#'                            data=bmi,
#'                            message=FALSE)
#' # Save the proposed 'mice' options as a 'miprop' object
#' ## (suppressing the message)
#' miprop <- proposeMI(mimodobj=mimod_bmi7,
#'                     data=bmi,
#'                     message=FALSE,
#'                     plot = FALSE)
#' # Create the set of imputed datasets using the proposed 'mice' options
#' imp <- doMImice(miprop,123)
#'
#' # Additionally, fit the substantive model to each imputed dataset and display
#' ## the pooled results
#' doMImice(miprop, 123, substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")
doMImice <- function(mipropobj, seed, substmod = " ", message = TRUE) {

  mids <- mice::mice(
        data = mipropobj$data,
        m = mipropobj$m,
        method = mipropobj$method,
        formulas = mipropobj[["formulas"]],
        maxit = 10,
        printFlag = FALSE,
        seed = seed)

  #If a substantive model is specified, calculate the pooled estimates
  if(substmod != " "){
    mipo <- mice::pool(with(mids,parse(text=substmod, keep.source=FALSE)))
    result <- paste("Given the substantive model:",
                    substmod,
"\n, multiple imputation estimates are as follows: \n \n",
              paste0(gsub(" ", "@",utils::capture.output(summary(mipo,conf.int=TRUE))),prefix="\n",collapse = "\n"),
              collapse = "\n")
  }
  else {
    result <- paste("Now you have created your multiply imputed datasets, you can perform your analysis and pool the results using the 'mice' functions 'with()' and 'pool()'", collapse = "\n")
    }

  if(message) {message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))}

  invisible(mids)

}
