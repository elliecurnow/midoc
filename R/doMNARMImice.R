#' Performs not-at-random multiple imputation
#'
#' Creates multiple imputations using \link[mice]{mice}. Imputations are based
#' on the options and dataset specified by a call to \link[midoc]{proposeMI},
#' and additionally on the specified missing not at random (MNAR) mechanism. If
#' a substantive model is specified, also calculates the pooled estimates using
#' \link[mice]{pool}.
#'
#' Imputation is performed using the NARFCS procedure (Tompsett et al, 2018) for
#' the specified variable. See \link[mice]{mice.impute.mnar.logreg} for further
#' details. All other partially observed variables are assumed to be missing at
#' random (MAR) and imputed using the method(s) specified for the 'miprop'
#' object.
#'
#' @param mipropobj An object of type 'miprop', created by a call to 'proposeMI'
#' @param mnardep The partially observed variable to be imputed under MNAR,
#'   specified as a string
#' @param mnardelta The desired sensitivity (delta) parameter as a function
#'   of other variables and values, specified as a string
#' @param seed An integer that is used to set the seed of the 'mice' call
#' @param substmod Optionally, a symbolic description of the substantive model
#'   to be fitted, specified as a string; if supplied, the model will be fitted
#'   to each imputed dataset and the results pooled
#' @param message If TRUE (the default), displays a message summarising the
#'   analysis that has been performed; use message = FALSE to suppress the
#'   message
#'
#' @return A 'mice' object of class 'mids' (the multiply imputed datasets).
#'   Optionally, a message summarising the analysis that has been performed.
#'
#' @export
#'
#' @references Tompsett D, Leacy F, Moreno-Betancur M, Heron J, & White IR.
#'   2018. On the use of the not-at-random fully conditional specification
#'   (NARFCS) procedure in practice. Statistics in Medicine.
#'   <doi:10.1002/sim.7643>
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
#' # Create the set of imputed datasets using the proposed 'mice' options and
#' # desired MNAR mechanism (decreasing imputed values of bmi7 by 2 units)
#' imp <- doMNARMImice(mipropobj=miprop, mnardep="bmi7", mnardelta="-2", seed=123)
#'
#' # Additionally, fit the substantive model to each imputed dataset and display
#' ## the pooled results
#' doMNARMImice(mipropobj=miprop, mnardep="bmi7", mnardelta="-2", seed=123,
#'              substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")
doMNARMImice <- function(mipropobj, mnardep, mnardelta, seed, substmod = " ", message = TRUE) {

  # Update method for mnardep variable
  if(length(mipropobj[["formulas"]])==1){
    miprop_count <- 1
  } else {
    miprop_count <- length(mipropobj[["formulas"]])
  }
  method <- mipropobj$method

  for (i in 1:miprop_count){
    if (substr(mipropobj[["formulas"]][[i]],1,1000)[2] == mnardep){
      method[i] <- paste("mnar.",mipropobj$method[[i]],sep="")
    } else {
      method[i] <- mipropobj$method[[i]]
    }
  }

  # Define blots for mnardep variable
  blots <- list(mnardep=list(ums=mnardelta))
  names(blots) <- mnardep

  #Perform NARFCS
  mids <- mice::mice(
        data = mipropobj$data,
        m = mipropobj$m,
        method = method,
        blots=blots,
        formulas = mipropobj[["formulas"]],
        maxit = 10,
        printFlag = FALSE,
        seed = seed)

  #If a substantive model is specified, calculate the pooled estimates
  if(substmod != " "){
    mipo <- mice::pool(with(mids,parse(text=substmod, keep.source=FALSE)))
    result <- paste("Given the substantive model:",
                    substmod,
                    "\n with missing not at random sensitivity parameter =",
                    mnardelta,
                    "for",
                    mnardep,
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
