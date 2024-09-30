#' Inspect multiple imputation model
#'
#' Check multiple imputation is valid under the proposed imputation model and
#' directed acyclic graph (DAG). Validity means that the proposed approach will
#' allow unbiased estimation of the estimand(s) of interest, including
#' regression parameters, associations, and causal effects. The imputation model
#' should include all other analysis model variables as predictors, as well as
#' any auxiliary variables. The DAG should include all observed and unobserved
#' variables related to the analysis model variables and their missingness, as
#' well as all required missingness indicators.
#'
#' In principle, multiple imputation is valid if each partially observed
#' variable is unrelated to its own missingness, given its imputation model
#' predictors.
#'
#' @param dep The partially observed variable to be imputed, specified as a
#'   string
#' @param preds The imputation model predictor(s), specified as a string (space
#'   delimited)
#' @param r_dep The partially observed variable's missingness indicator,
#'   specified as a string
#' @param mdag The DAG, specified as a string using \link[dagitty]{dagitty}
#'   syntax
#'
#' @return A message indicating whether multiple imputation is valid under the
#'   proposed DAG and imputation model
#' @export
#'
#' @references Curnow E, Tilling K, Heron JE, Cornish RP, Carpenter JR. 2023.
#'   Multiple imputation of missing data under missing at random: including a
#'   collider as an auxiliary variable in the imputation model can induce bias.
#'   Frontiers in Epidemiology. <doi:10.3389/fepid.2023.1237447>
#'
#' @examples
#' # Example DAG for which multiple imputation is valid
#' checkMI(dep="bmi7", preds="matage mated pregsize", r_dep="r",
#'         mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'               sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
#'               pregsize -> bwt sep_unmeas -> bwt")
#'
#' # Example DAG for which multiple imputation is not valid, due to a collider
#' checkMI(dep="bmi7", preds="matage mated bwt", r_dep="r",
#'         mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'               sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
#'               pregsize -> bwt sep_unmeas -> bwt")
checkMI <- function(dep, preds, r_dep, mdag) {
  mdagspec <- paste('dag {',mdag,'}')
  predsvec <- unlist(strsplit(preds," "))
  #If r_dep does not depend on dep conditional on predictors, then MI is valid
  if(dagitty::dseparated(dagitty::dagitty(mdagspec, layout=T), dep, r_dep, predsvec)){
    result <- paste("Based on the proposed directed acyclic graph (DAG), the incomplete variable and its missingness indicator are independent given imputation model predictors. Hence, multiple imputation methods which assume data are missing at random are valid in principle.", collapse="\n")
  } else {
      result1 <- paste("Based on the proposed directed acyclic graph (DAG), the incomplete variable and its missingness indicator are not independent given imputation model predictors. Hence, multiple imputation methods which assume data are missing at random are not valid. \n \nConsider using a different imputation model and/or strategy (e.g. not-at-random fully conditional specification).",
        collapse="\n")
      adjsets_r <- dagitty::adjustmentSets(mdagspec,exposure=c(predsvec,r_dep),outcome=dep,type = "all")
      adjsets_dep <- dagitty::adjustmentSets(mdagspec,exposure=c(predsvec,dep),outcome=r_dep,type = "all")
      if(length(adjsets_r)>0) (adjsets <- adjsets_r)
        else (adjsets <- adjsets_dep)
      if(length(adjsets)>0){
        result2 <- paste("For example, the incomplete variable and its missingness indicator are independent if, in addition to the specified predictors, the following sets of variables are included as predictors in the imputation model (note that this list is not necessarily exhaustive, particularly if your DAG is complex):\n \n",
                          paste0(adjsets, prefix="\n", collapse = "\n"),collapse = "\n")
        #print(adjsets)
        }
    result <- paste(result1, "\n", result2, collapse = "\n")
  }
  message(paste(strwrap(result),collapse="\n"))
}



