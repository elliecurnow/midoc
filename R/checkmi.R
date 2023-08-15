#' Check multiple imputation is valid
#'
#' @param dep the partially observed variable, specified as a string
#' @param preds the imputation model predictor(s), specified as a string (space delimited)
#' @param r the partially observed variable's missingness indicator, specified as a string
#' @param mdag the directed acyclic graph specified as a string using "dagitty" syntax, including all observed and
#' unobserved variables related to the analysis model variables and their missingness
#'
#' @return A message indicating whether multiple imputation is valid under the proposed missingness
#' DAG and imputation model
#' @export
#'
#' @examples
#' checkmi(dep="bmi7", preds="matage mated pregsize", r="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R mated -> bwt
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt  U -> R  U -> mated")
#' checkmi(dep="bmi7", preds="matage mated bwt", r="R",
#'  mdag="mated -> bmi7 mated -> matage matage -> bmi7 mated -> R mated -> bwt
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt  U -> R  U -> mated")
checkmi <- function(dep, preds, r, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  predsvec <- unlist(strsplit(preds," "))
  #If r does not depend on dep conditional on predictors, then MI is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), dep, r, predsvec)){
    cat("The incomplete variable and its missingness indicator are independent given imputation model predictors.
Hence, multiple imputation methods which assume data are missing at random are valid in principle.", fill=TRUE)
  } else {
    cat("The incomplete variable and its missingness indicator are not independent given imputation model predictors.
Hence, multiple imputation methods which assume data are missing at random may not be valid.","\n",
"Consider using a different strategy (e.g. not-at-random fully conditional specification)
and/or imputation model.","\n",fill=TRUE)
      adjsets <- dagitty::adjustmentSets(mdagspec,exposure=c(predsvec,r),outcome=dep,type = "all")
      if(length(adjsets)>0){
        cat("For example, the incomplete variable and its missingness indicator are independent if,
in addition to the specified predictors, the following sets of variables are included
as predictors in the imputation model:",fill=TRUE)
      print(adjsets)
        }
      }
    }



