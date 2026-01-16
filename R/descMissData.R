#' Lists missing data patterns in the specified dataset
#'
#' Summarises the missing data patterns in the specified dataset. Each row in
#' the output corresponds to a missing data pattern (1=observed, 0=missing). The
#' number and percentage of observations is also displayed for each missing data
#' pattern. The first column indicates the number of missing data patterns. The
#' second column refers to the analysis model outcome ('y'), with all other
#' variables ('covs') displayed in subsequent columns. Alternatively, 'y' can
#' indicate the primary variable of interest, e.g. 'y' could refer to an
#' exposure or intervention, with all other variables listed in 'covs'.
#'
#' @param y The analysis model outcome variable(s), specified as a string (space
#'   delimited) or a list
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited) or a list
#' @param by Optional stratification variable(s), specified as a string (space
#'   delimited) or a list of factors; if specified, the data are subsetted by
#'   the values of the factor(s) and missing data patterns are displayed for
#'   each subset in turn
#' @param data A data frame containing the specified analysis model outcome,
#'   covariate(s), and if specified, stratification variable(s)
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
descMissData <- function(y, covs, by=NULL, data, plot=FALSE) {

  ylist <- unlist(strsplit(y," "))
  covslist <- unlist(strsplit(covs," "))

  if(is.null(by)){
    mdtab <- list(mice::md.pattern(data[,c(ylist,covslist)],plot=plot))
  } else {
    bylist <- unlist(strsplit(by," "))
    mdtab <- by(data[,c(ylist,covslist)],data[,c(bylist)],
                function(x) mice::md.pattern(x,plot=plot))
    names(dimnames(mdtab)) = bylist
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
    mdtmp2 <- mdtmp[,c(ylist,covslist)]

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



