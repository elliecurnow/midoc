#' Inspect complete records analysis model
#'
#' Check complete records analysis is valid under the proposed analysis model
#' and directed acyclic graph (DAG). Validity means that the proposed approach
#' will allow unbiased estimation of the estimand(s) of interest, including
#' regression parameters, associations, and causal effects.
#'
#' The DAG should include all observed and unobserved variables related to the
#' analysis model variables and their missingness, as well as all required
#' missingness indicators.
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
#' @references Hughes R, Heron J, Sterne J, Tilling K. 2019. Accounting for
#'   missing data in statistical analyses: multiple imputation is not always the
#'   answer. Int J Epidemiol. <doi:10.1093/ije/dyz032>
#'
#'   Bartlett JW, Harel O, Carpenter JR. 2015. Asymptotically Unbiased
#'   Estimation of Exposure Odds Ratios in Complete Records Logistic Regression.
#'   Am J Epidemiol. <doi:10.1093/aje/kwv114>
#'
#' @examples
#' # Example DAG for which complete records analysis is not valid, but could be
#' ## valid for a different set of covariates
#' checkCRA(y="bmi7", covs="matage", r_cra="r",
#'          mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                sep_unmeas -> mated sep_unmeas -> r")
#' # For the DAG in the example above, complete records analysis is valid
#' ## if a different set of covariates is used
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'          mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                sep_unmeas -> mated sep_unmeas -> r")
#'
#' # Example DAG for which complete records is not valid, but could be valid
#' ## for a different estimand
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'          mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                sep_unmeas -> mated sep_unmeas -> r matage -> bmi3
#'                mated -> bmi3 bmi3 -> bmi7 bmi3 -> r")
#'
#' # Example DAG for which complete records analysis is never valid
#' checkCRA(y="bmi7", covs="matage mated", r_cra="r",
#'          mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                sep_unmeas -> mated sep_unmeas -> r bmi7 -> r")
checkCRA <- function(y, covs, r_cra, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  covsvec <- unlist(strsplit(covs," "))
  #If r does not depend on y conditional on covariates, then CRA is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), y, r_cra, covsvec)){
    result <- paste("Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are independent given analysis model covariates. Hence, complete records analysis is valid.", collapse="\n")
  }
  else {
    result1 <- paste("Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are not independent given analysis model covariates. Hence, in general, complete records analysis is not valid. \n \nIn special cases, depending on the type of analysis model and estimand of interest, complete records analysis may still be valid. See, for example, Bartlett et al. (2015) (https://doi.org/10.1093/aje/kwv114) for further details.\n",collapse = "\n")

    adjsets <- dagitty::adjustmentSets(mdagspec,exposure=c(covsvec,r_cra),outcome=y,type = "all")
    adjsetsfull <- dagitty::adjustmentSets(mdagspec,exposure=r_cra,outcome=y,type = "all")
    if(length(adjsets)==0 & length(adjsetsfull)==0){
      result2 <- paste("There are no other variables which could be added to the model to make the analysis model outcome and complete record indicator conditionally independent. Consider using a different strategy e.g. multiple imputation.",collapse = "\n")
    }
    if(length(adjsets)==0 & length(adjsetsfull)>0){
      result2 <- paste("There are no other variables which could be added to the model to make the analysis model outcome and complete record indicator conditionally independent, without changing the estimand of interest. Consider using a different strategy e.g. multiple imputation. \n \nAlternatively, consider whether a different estimand could be of interest. For example, the analysis model outcome and complete record indicator are independent given each of the following sets of variables:\n \n",
          paste0(adjsetsfull, prefix="\n", collapse = "\n"),collapse = "\n")
      #print(adjsetsfull)
    }
    if(length(adjsets)>0){
      result2 <- paste("Consider using a different analysis model and/or strategy, e.g. multiple imputation. \n \nFor example, the analysis model outcome and complete record indicator are independent if, in addition to the specified covariates, the following sets of variables are included as covariates in the analysis model (note that this list is not necessarily exhaustive, particularly if your DAG is complex):\n \n", paste0(adjsets, prefix="\n", collapse = "\n"), collapse = "\n")
      #print(adjsets)
    }
  result <- paste(result1, "\n", result2, collapse="\n")
  }
  message(paste(strwrap(result),collapse="\n"))
}


