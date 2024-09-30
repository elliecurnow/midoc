#' Compares data with proposed DAG
#'
#' Explore whether relationships between fully observed variables in the
#' specified dataset are consistent with the proposed directed acyclic graph
#' (DAG) using \link[dagitty]{localTests} functionality.
#'
#' @param mdag The DAG, specified as a string using \link[dagitty]{dagitty}
#'   syntax
#' @param data A data frame containing all the variables stated in the DAG. All
#'   ordinal variables must be integer-coded and all categorical variables must
#'   be dummy-coded.
#'
#' @return A message indicating whether the relationships between fully observed
#'   variables in the specified dataset are consistent with the proposed DAG

#' @export
#'
#' @examples
#' exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7
#'                  sep_unmeas -> mated sep_unmeas -> r",
#'            data=bmi)
exploreDAG <- function(mdag, data) {
  mod <- dagitty::dagitty(paste('dag {',mdag,'}'), layout=T)

  # Keep fully observed variables from dataset
  mdlist <- mice::md.pattern(data,plot=FALSE)
  complist <- mdlist[nrow(mdlist),]
  compvar <- names(complist[complist==0])
  compdata <- data[,c(compvar)]

  tests <- dagitty::impliedConditionalIndependencies(mod)
  comptests <- Filter(function(x) all(x$X %in% compvar) & all(x$Y %in% compvar) & all(x$Z %in% compvar),
                  tests)
  comptestsres <- dagitty::localTests(x=mod,data=compdata,tests=comptests,type="cis.pillai",abbreviate.names=FALSE)
  result1 <- paste("The proposed directed acyclic graph (DAG) implies the following conditional independencies (where, for example, 'X _||_ Y | Z' should be read as 'X is independent of Y conditional on Z'). Note that variable names are abbreviated: \n \n",
                   paste0(utils::capture.output(tests), prefix="\n",collapse = "\n"),collapse = "\n")

  if(nrow(comptestsres)==0){
    result2 <- paste("None of the fully observed variables are conditionally independent.\nHence, no consistency checks will be performed. \nConsider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g. avoiding perfect prediction.",collapse = "\n")
  }else{
    result2 <- paste("These (conditional) independence statements are explored below using the canonical correlations approach for mixed data. See ??dagitty::localTests for further details. \nResults are shown for variables that are fully observed in the specified dataset.\nThe null hypothesis is that the stated variables are (conditionally) independent. \n \n",
                     paste0(gsub(" ", "@",utils::capture.output(comptestsres)),prefix="\n",collapse = "\n"),
"\n \nInterpretation: A small p-value means the stated variables may not be (conditionally) independent in the specified dataset: your data may not be consistent with the proposed DAG. A large p-value means there is little evidence of inconsistency between your data and the proposed DAG. \n \nNote that these results assume that relationships between variables are linear. Consider exploring the specification of each relationship in your model. \nAlso consider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g. avoiding perfect prediction.",collapse = "\n")
  }

  result <- paste(result1, "\n", result2, collapse = "\n")
  message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))
}
