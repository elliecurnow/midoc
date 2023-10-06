#' Check multiple imputation is valid
#'
#' @param dep the partially observed variable, specified as a string
#' @param preds the imputation model predictor(s), specified as a string (space delimited)
#' @param r_dep the partially observed variable's missingness indicator, specified as a string
#' @param mdag the directed acyclic graph specified as a string using "dagitty" syntax, including all observed and
#' unobserved variables related to the analysis model variables and their missingness
#'
#' @return A message indicating whether multiple imputation is valid under the proposed missingness
#' DAG and imputation model
#' @export
#'
#' @examples
#' checkMI(dep="bmi7", preds="matage mated pregsize", r_dep="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R mated -> bwt
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt  U -> R  U -> mated")
#' checkMI(dep="bmi7", preds="matage mated bwt", r_dep="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R mated -> bwt
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt  U -> R  U -> mated")
#' checkMI(dep="matage", preds="bmi7 mated bwt", r_dep="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R mated -> bwt
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt  U -> R  U -> mated")
checkMI <- function(dep, preds, r_dep, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  predsvec <- unlist(strsplit(preds," "))
  #If r_dep does not depend on dep conditional on predictors, then MI is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), dep, r_dep, predsvec)){
    cat("The incomplete variable and its missingness indicator are independent
given imputation model predictors. Hence, multiple imputation methods which
assume data are missing at random are valid in principle.", fill=TRUE)
  } else {
    cat("The incomplete variable and its missingness indicator are not independent
given imputation model predictors. Hence, multiple imputation methods which assume
data are missing at random may not be valid.","\n",
"Consider using a different imputation model and/or strategy (e.g. not-at-random
fully conditional specification).","\n",fill=TRUE)
      adjsets_r <- dagitty::adjustmentSets(mdagspec,exposure=c(predsvec,r_dep),outcome=dep,type = "all")
      adjsets_dep <- dagitty::adjustmentSets(mdagspec,exposure=c(predsvec,dep),outcome=r_dep,type = "all")
      if(length(adjsets_r)>0) (adjsets <- adjsets_r)
        else (adjsets <- adjsets_dep)
      if(length(adjsets)>0){
        cat("For example, the incomplete variable and its missingness indicator are
independent if, in addition to the specified predictors, the following sets of
variables are included as predictors in the imputation model:",fill=TRUE)
        print(adjsets)
            }
         }
}



