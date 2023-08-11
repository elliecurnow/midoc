#' Check complete records analysis is valid
#'
#' @param y the analysis model outcome, specified as a string
#' @param covs the analysis model covariate(s), specified as a string (space delimited)
#' @param r the complete record indicator, specified as a string
#' @param mdag the directed acyclic graph specified as a string using "dagitty" syntax, including all observed and
#' unobserved variables related to the analysis model variables and their missingness
#'
#' @return A message indicating whether complete records analysis is valid under the proposed missingness
#' DAG and analysis model outcome and covariate(s)
#' @export
#'
#' @examples
#' checkcra(y="edscore14", covs="iq8", r="r", mdag="iq8 -> edscore14 iq8 -> r")
#' checkcra(y="edscore14", covs="iq8", r="r",
#'  mdag="smoking -> iq8 -> edscore14 smoking -> edscore14 smoking -> r")
#' checkcra(y="edscore14", covs="iq8 smoking", r="r",
#'  mdag="smoking -> iq8 -> edscore14 smoking -> edscore14 smoking -> r")
checkcra <- function(y, covs, r, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  covsvec <- unlist(strsplit(covs," "))
  #If r does not depend on y conditional on covariates, then CRA is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), y, r, covsvec)){
    cat("The analysis model outcome and complete record indicator are independent given analysis model covariates.
Hence, complete records analysis is valid.", fill=TRUE)
  }
  else {
    cat("The analysis model outcome and complete record indicator are not independent given analysis model covariates.
Hence, complete records analysis may not be valid.","\n",fill=TRUE)
    adjsets <- dagitty::adjustmentSets(mdagspec,exposure=c(covsvec,r),outcome=y,type = "all")
    if(length(adjsets)==0){
      cat("There are no other variables which could be added to the model to make
the analysis model outcome and complete record indicator conditionally independent.",
          "\n",
"Consider using a different strategy e.g. multiple imputation.",fill=TRUE)
    } else {
      cat("Consider using a different different strategy, e.g. multiple imputation, and/or analysis model.",
          "\n",
"For example, the analysis model outcome and complete record indicator are independent if,
in addition to the specified covariates, the following sets of variables are included
as covariates in the analysis model:","\n",fill=TRUE)
print(adjsets)
    }
  }
}

#Add note on special case where missingness of independent of model covariates and depends on a cause of Y not Y itself
