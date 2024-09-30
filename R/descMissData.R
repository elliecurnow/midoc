#' Lists missing data patterns in the specified dataset
#'
#' This function summarises the missing data patterns in the specified dataset.
#' Each row in the output corresponds to a missing data pattern (1=observed,
#' 0=missing). The number and percentage of observations is also displayed for
#' each missing data pattern. The first column indicates the number of missing
#' data patterns. The second column refers to the analysis model outcome
#' ('y'), with all other variables ('covs') displayed in subsequent columns.
#' Alternatively, 'y' can be used to display the primary variable of interest,
#' e.g. 'y' could refer to the exposure, with all other variables listed in
#' 'covs'.
#'
#' @param y The analysis model outcome, specified as a string
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited)
#' @param data A data frame containing the specified analysis model outcome and
#'   covariate(s)
#' @param plot If TRUE, displays a plot using \link[mice]{md.pattern} to
#' visualise the missing data patterns; use plot = FALSE (the default) to
#' disable the plot
#'
#' @return A summary of the missing data patterns
#' @export
#'
#' @examples
#' descMissData(y="bmi7", covs="matage mated", data=bmi)
#' descMissData(y="bmi7", covs="matage mated pregsize bwt", data=bmi, plot=TRUE)
descMissData <- function(y, covs, data, plot=FALSE) {
  covslist <- unlist(strsplit(covs," "))
  mdtab <- mice::md.pattern(data[,c(y,covslist)],plot=plot)

  #Only print if dataset is not completely observed - if it is, print output as per md.pattern
  if (mdtab[nrow(mdtab),ncol(mdtab)] != 0){
    #reorder so outcome is listed first
    mdtab2 <- mdtab[1:nrow(mdtab)-1,c(y,covslist)]
    n <- as.numeric(row.names(mdtab[1:nrow(mdtab)-1,]))
    pct <-  round(n*100/sum(n))
    pattern <- 1:(nrow(mdtab)-1)
    result<-data.frame(pattern,mdtab2,n,pct)
    row.names(result) <- NULL
    return(result)
  }
}


