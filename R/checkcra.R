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
#' checkCRA(y="bmi7", covs="matage", r="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R")
#' checkCRA(y="bmi7", covs="matage mated", r="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R")
#' checkCRA(y="bmi7", covs="matage mated", r="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R bmi7 -> R")
checkCRA <- function(y, covs, r, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  covsvec <- unlist(strsplit(covs," "))
  #If r does not depend on y conditional on covariates, then CRA is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), y, r, covsvec)){
    cat("The analysis model outcome and complete record indicator are independent
given analysis model covariates. Hence, complete records analysis is valid.", fill=TRUE)
  }
  else {
    cat("The analysis model outcome and complete record indicator are not independent
given analysis model covariates. Hence, complete records analysis may not be valid.",
        "\n",fill=TRUE)
    adjsets <- dagitty::adjustmentSets(mdagspec,exposure=c(covsvec,r),outcome=y,type = "all")
    if(length(adjsets)==0){
      cat("There are no other variables which could be added to the model to make
the analysis model outcome and complete record indicator conditionally independent.",
          "\n",
"Consider using a different strategy e.g. multiple imputation.",fill=TRUE)
    } else {
      cat("Consider using a different strategy, e.g. multiple imputation,
and/or analysis model.",
          "\n",
"For example, the analysis model outcome and complete record indicator are independent
if, in addition to the specified covariates, the following sets of variables are included
as covariates in the analysis model:",
          "\n",fill=TRUE)
print(adjsets)
    }
  }
}


