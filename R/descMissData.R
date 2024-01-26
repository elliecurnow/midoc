#' Lists missing data patterns in the specified dataset
#'
#' This function summarises the missing data patterns in the specified dataset.
#' Each row in the output corresponds to a missing data pattern (1=observed,
#' 0=missing). The number and percentage of observations is also displayed for
#' each missing data pattern.
#'
#' @param y The analysis model outcome, specified as a string
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited)
#' @param data A data frame containing the specified analysis model outcome and
#'   covariate(s)
#'
#' @return A summary of the missing data patterns
#' @export
#'
#' @examples
#' descMissData(y="bmi7", covs="matage mated", data=bmi)
#' descMissData(y="bmi7", covs="matage mated pregsize bwt", data=bmi)
descMissData <- function(y, covs, data) {
  covslist <- unlist(strsplit(covs," "))
  mdtab <- mice::md.pattern(data[,c(y,covslist)],plot=FALSE)
  #reorder so outcome is listed first
  mdtab2 <- mdtab[1:nrow(mdtab)-1,c(y,covslist)]
  n <- as.numeric(row.names(mdtab[1:nrow(mdtab)-1,]))
  pct <-  round(n*100/sum(n))
  pattern <- 1:(nrow(mdtab)-1)
  print(data.frame(pattern,mdtab2,n,pct),row.names=FALSE)
}


