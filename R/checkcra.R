#' Inspect complete records analysis model
#'
#' Check complete records analysis is valid under the proposed analysis model
#' and directed acyclic graph (DAG). The DAG should include all observed and
#' unobserved variables related to the analysis model variables and their
#' missingness, as well as all required missingness indicators.
#'
#' In general, complete records analysis is valid if the analysis model outcome
#' and complete record indicator are unrelated, conditional on the specified
#' covariates (in causal inference this is referred to as 'd-separation').
#'
#' @param y The analysis model outcome, specified as a string
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited)
#' @param r_cra The complete record indicator, specified as a string
#' @param mdag The DAG, specified as a string using \link[dagitty]{dagitty}
#'   syntax
#'
#' @return A message indicating whether complete records analysis is valid under
#'   the proposed DAG and analysis model outcome and covariate(s)
#' @export
#'
#' @examples
#' checkCRA(y="bmi7", covs="matage", r_cra="r",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 U -> mated U -> r")
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 U -> mated U -> r")
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 U -> mated U -> r bmi7 -> r")
checkCRA <- function(y, covs, r_cra, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  covsvec <- unlist(strsplit(covs," "))
  #If r does not depend on y conditional on covariates, then CRA is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), y, r_cra, covsvec)){
    cat(strwrap("The analysis model outcome and complete record indicator are independent
given analysis model covariates. Hence, complete records analysis is valid."),"\n",fill=TRUE)
  }
  else {
    cat(strwrap("The analysis model outcome and complete record indicator are not independent
given analysis model covariates. Hence, complete records analysis may not be valid."),"\n", fill=TRUE)

    adjsets <- dagitty::adjustmentSets(mdagspec,exposure=c(covsvec,r_cra),outcome=y,type = "all")
    if(length(adjsets)==0){
      cat(strwrap("There are no other variables which could be added to the model to make
the analysis model outcome and complete record indicator conditionally independent.
\nConsider using a different strategy e.g. multiple imputation."),fill=TRUE)
    } else {
      cat(strwrap("Consider using a different analysis model and/or strategy, e.g. multiple imputation.
\nFor example, the analysis model outcome and complete record indicator are independent
if, in addition to the specified covariates, the following sets of variables are included
as covariates in the analysis model (note that this list is not necessarily exhaustive,
particularly if your DAG is complex):"),"\n", fill=TRUE)
      print(adjsets)
      }
  }
}


