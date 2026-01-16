#' Compares data with proposed DAG
#'
#' Explore the relationships implied by the proposed directed acyclic graph
#' (DAG). Optionally, if a dataset is supplied, explore whether relationships
#' between fully observed variables in the specified dataset are consistent with
#' the proposed DAG.
#'
#' @param mdag The DAG, specified as a string using \link[dagitty]{dagitty}
#'   syntax, or as a \link[dagitty]{dagitty} graph object
#' @param data Optionally, a data frame containing all the variables stated in
#'   the DAG. All ordinal variables must be integer-coded and all categorical
#'   variables must be dummy-coded.
#'
#' @return A message listing the pairs of variables that are implied to be
#'   independent (possibly conditional on other variables) by the proposed DAG.
#'   Optionally, if a dataset is supplied, a message indicating whether the
#'   relationships between fully observed variables in the specified dataset are
#'   consistent with the proposed DAG.

#' @export
#'
#' @examples
#' exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                  sep_unmeas -> mated sep_unmeas -> r",
#'            data=bmi)
exploreDAG <- function(mdag, data=NULL) {

  #Check whether input DAG is a dagitty object or not
  if(dagitty::is.dagitty(mdag)){
    mod <- mdag
  } else {
    mod <- dagitty::dagitty(mdag, layout=T)
  }

  tests <- dagitty::impliedConditionalIndependencies(mod)
  result1 <- paste("The proposed directed acyclic graph (DAG) implies the following pairs of variables are (conditionally) independent
(where, for example, 'X _||_ Y | Z' should be read as 'X is independent of Y conditional on Z').
Note that variable names are abbreviated. Consider whether these (conditional) independencies are plausible for your study, and update your DAG accordingly: \n \n",
                   paste0(utils::capture.output(tests), prefix="\n",collapse = "\n"),collapse = "\n")

  #Explore observed data if supplied
  if (!is.null(data)) {
    # Keep fully observed variables from dataset
    mdlist <- mice::md.pattern(data,plot=FALSE)
    complist <- mdlist[nrow(mdlist),]
    compvar <- names(complist[complist==0])
    compdata <- data[,c(compvar)]

    # Perform cond independence tests on fully observed variables only
    comptests <- Filter(function(x) all(x$X %in% compvar) & all(x$Y %in% compvar) & all(x$Z %in% compvar),
                    tests)
    comptestsres <- dagitty::localTests(x=mod,data=compdata,tests=comptests,type="cis.pillai",abbreviate.names=FALSE)


    if(nrow(comptestsres)==0){
      result2 <- paste("None of the fully observed variables are conditionally independent.\nHence, no consistency checks will be performed. \nConsider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g. avoiding perfect prediction.",collapse = "\n")
    }else{
      result2 <- paste("These (conditional) independence statements are explored below using the canonical correlations approach for mixed data. See ??dagitty::localTests for further details. \nResults are shown for variables that are fully observed in the specified dataset.\nThe null hypothesis is that the stated variables are (conditionally) independent. \n \n",
                       paste0(gsub(" ", "@",utils::capture.output(comptestsres)),prefix="\n",collapse = "\n"),
  "\n \nInterpretation: A strong correlation means the stated variables may not be (conditionally) independent in the specified dataset: your data may not be consistent with the proposed DAG. A weak correlation means there is little evidence of inconsistency between your data and the proposed DAG. \n \nNote that there may also be other DAGs which your data are consistent with. Also note that these results assume that relationships between variables are linear. Consider exploring the specification of each relationship in your model. \nAlso consider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g. avoiding perfect prediction.",collapse = "\n")
    }

    result <- paste(result1, "\n", result2, collapse = "\n")
  } else result <- result1

  message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))
}
