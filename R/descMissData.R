#' Lists missing data patterns in the specified dataset
#'
#' This function summarises the missing data patterns in the specified dataset.
#' Each row in the output corresponds to a missing data pattern (1=observed,
#' 0=missing). The number and percentage of observations is also displayed for
#' each missing data pattern. The first column indicates the number of missing
#' data patterns. The second column refers to the analysis model outcome ('y'),
#' with all other variables ('covs') displayed in subsequent columns.
#' Alternatively, 'y' can be used to display the primary variable of interest,
#' e.g. 'y' could refer to the exposure, with all other variables listed in
#' 'covs'.
#'
#' @param y The analysis model outcome, specified as a string
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited)
#' @param data A data frame containing the specified analysis model outcome,
#'   covariate(s), and if specified, stratification variable(s)
#' @param by Optional stratification variable(s), which must be a factor, or
#'   list of factors; if specified, the data are subsetted by the values of the
#'   factor(s) and missing data patterns are displayed for each subset in turn
#' @param plot If TRUE, displays a plot using \link[mice]{md.pattern} to
#'   visualise the missing data patterns; if stratification variable(s) are
#'   specified, a separate plot will be displayed for each subset; use plot =
#'   FALSE (the default) to disable the plot
#'
#' @return A summary of the missing data patterns
#' @export
#'
#' @examples
#' descMissData(y="bmi7", covs="matage mated", data=bmi)
#' descMissData(y="bmi7", covs="matage mated bwt", by="pregsize", data=bmi)
descMissData <- function(y, covs, data, by=NULL, plot=FALSE) {
  covslist <- unlist(strsplit(covs," "))

  if(is.null(by)){
    mdtab <- list(mice::md.pattern(data[,c(y,covslist)],plot=plot))
  } else {
    mdtab <- by(data[,c(y,covslist)],data[,c(by)],
                function(x) mice::md.pattern(x,plot=plot))
    names(dimnames(mdtab)) = by
  }

  for (i in 1:length(mdtab)){

    #Removed next condition because summary is useful even if no missing data
    #if (mdtab[[i]][nrow(mdtab[[i]]),ncol(mdtab[[i]])] != 0){
    mdtmp <- mdtab[[i]]

    #Summarise
    n <- as.numeric(row.names(mdtmp)[1:nrow(mdtmp)])
    pct <-  round(n*100/sum(n,na.rm=TRUE))
    pattern <- 1:(nrow(mdtmp))

    #Reorder so outcome is listed first
    mdtmp2 <- mdtmp[,c(y,covslist)]

    #Combine with summary variables
    mdtmp3 <- cbind(pattern,mdtmp2,n,pct)
    row.names(mdtmp3) <- NULL
    mdtab[[i]] <- mdtmp3[1:nrow(mdtmp)-1,]
  }

  return(mdtab)
    #result<-data.frame(pattern,mdtmp2,n,pct)
    #row.names(result) <- NULL
    #return(result)
}



