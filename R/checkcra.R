#' Inspect complete records analysis model
#'
#' Check complete records analysis is valid under the proposed analysis model
#' and directed acyclic graph (DAG). The DAG should include all observed and
#' unobserved variables related to the analysis model variables and their
#' missingness, as well as all required missingness indicators.
#'
#' In general, complete records analysis is valid if the analysis model outcome
#' and complete record indicator are unrelated, conditional on the specified
#' covariates. This is determined using the proposed DAG by checking whether the
#' analysis model and complete record indicator are 'd-separated', given the
#' covariates.
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
#'   mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas ->
#'   mated sep_unmeas -> r")
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'   mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas ->
#'   mated sep_unmeas -> r")
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'   mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas ->
#'   mated sep_unmeas -> r bmi7 -> r")
checkCRA <- function(y, covs, r_cra, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  covsvec <- unlist(strsplit(covs," "))
  #If r does not depend on y conditional on covariates, then CRA is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), y, r_cra, covsvec)){
    cat(strwrap("Based on the proposed directed acyclic graph (DAG),
the analysis model outcome and complete record indicator are independent
given analysis model covariates. Hence, complete records analysis is valid."),"\n",fill=TRUE)
  }
  else {
    cat(strwrap("Based on the proposed directed acyclic graph (DAG),
the analysis model outcome and complete record indicator are not independent
given analysis model covariates.
Hence, in general, complete records analysis is not valid."),"\n",
    strwrap("In special cases, depending on the type of analysis model and estimand of interest, complete
records analysis may still be valid. See, for example, Bartlett et al. (2015)
(https://doi.org/10.1093/aje/kwv114) for further details."),"\n", fill=TRUE)

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


