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
#' exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas ->
#'   mated sep_unmeas -> r", data=bmi)
#' exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas ->
#'   mated sep_unmeas -> r pregsize -> bmi7 pregsize -> bwt sep_unmeas -> bwt",
#'   data=bmi)
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
  if(nrow(comptestsres)==0){
    cat(strwrap("The proposed directed acyclic graph (DAG) implies the following
conditional independencies
(where, for example, 'X _||_ Y | Z' should be read as
'X is independent of Y conditional on Z'). Note that variable names are abbreviated:"),"\n",fill=TRUE)
    print(tests)
    cat("\n")
    cat(strwrap("None of the fully observed variables are conditionally independent.
    \nHence, no consistency checks will be performed. \nConsider whether
it is valid and possible to explore relationships between partially observed variables
using the observed data, e.g. avoiding perfect prediction."),"\n",fill=TRUE)
  }else{
    cat(strwrap("The proposed directed acyclic graph (DAG) implies that the
    following pairs of variables are independent, or conditionally independent.
    'X _||_ Y | Z' should be read as 'X is independent of Y conditional on Z'.
    Note that variable names are abbreviated:"),"\n",fill=TRUE)
    print(tests)
    cat("\n")
    cat(strwrap("These (conditional) independence statements are explored below
    using the canonical correlations approach for mixed data. See
                ??dagitty::localTests for further details."),"\n",
        strwrap("Results are shown for variables that are fully observed in the
        specified dataset."),
        strwrap("The null hypothesis is that the stated variables are (conditionally) independent."),"\n",fill=TRUE)
    print(dagitty::localTests(x=mod,data=compdata,tests=comptests,type="cis.pillai",abbreviate.names=FALSE))
    cat("\n", strwrap("Interpretation: A small p-value means the stated variables may not be (conditionally) independent
        in the specified dataset: your data may not be consistent with the proposed DAG.
        A large p-value means there is little evidence of inconsistency between your data
                 and the proposed DAG."),"\n",fill=TRUE)
    cat(strwrap("Note that these results assume that relationships between variables are linear.
Consider exploring the specification of each relationship in your model. \nAlso consider whether
it is valid and possible to explore relationships between partially observed variables
using the observed data, e.g. avoiding perfect prediction."),"\n",fill=TRUE)
  }
}
