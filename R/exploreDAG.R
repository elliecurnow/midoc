#' Compares data with proposed DAG
#'
#' Explore whether relationships between fully observed variables in the
#' specified dataset are consistent with the proposed directed acyclic graph
#' (DAG) using \link[dagitty]{localTests} functionality.
#'
#' @param mdag The DAG, specified as a string using \link[dagitty]{dagitty}
#'   syntax
#' @param data A data frame containing all the variables stated in the DAG
#'
#' @return A message indicating whether the relationships between fully observed
#'   variables in the specified dataset are consistent with the proposed DAG

#' @export
#'
#' @examples
#' exploreDAG(mdag="mated -> bmi7 mated -> matage matage -> bmi7 U -> mated U -> r", data=bmi)
#' exploreDAG(mdag="mated -> bmi7 mated -> matage matage -> bmi7 U -> mated U -> r
#'  pregsize -> bmi7 pregsize -> bwt  U -> bwt", data=bmi)
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

  cat(strwrap("Method for exploring the relationships between fully observed variables given
the proposed directed acyclic graph (DAG): linear conditional independence.
See ??dagitty::localTests for further details."),"\n",fill=TRUE)
  print(dagitty::localTests(x=mod,data=compdata,tests=comptests,type="cis",abbreviate.names=FALSE))
  cat("\n",strwrap("A large p-value for a conditional model means there is little evidence of
inconsistency between your data and the proposed DAG."),"\n",
strwrap("A small p-value for a conditional model means your data may not be consistent
with the proposed DAG and/or the model may be mis-specified.
Check the specification of your DAG and each relationship in your model."),"\n",
strwrap("Consider also exploring relationships with partially observed variables
using the observed data, if valid and possible, i.e. avoiding perfect prediction."),"\n",fill=TRUE)
}
