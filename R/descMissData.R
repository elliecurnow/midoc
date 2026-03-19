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
#' @param data A data frame containing the specified analysis model outcome,
#'   covariate(s), and if specified, stratification variable(s)
#' @param covs Optional analysis model covariate(s), specified as a string (space
#'   delimited) or a list
#' @param by Optional stratification variable(s), specified as a string (space
#'   delimited) or a list of factors; if specified, the data are subsetted by
#'   the values of the factor(s) and missing data patterns are displayed for
#'   each subset in turn; can only be used when the total number of variables
#'   listed in 'y' and 'covs' is greater than one
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
descMissData <- function(y, data, covs=NULL, by=NULL, plot=FALSE) {

  if(is.null(covs)){
    varlist <- unlist(strsplit(y," "))
  } else {
    varlist <- c(unlist(strsplit(y," ")), unlist(strsplit(covs," ")))
  }

  if(is.null(by)){
    #md.pattern requires >1 variable so manually run md.pattern if only 1 variable is specified
    if (length(varlist)==1){
      if (all(!is.na(data[,c(varlist)]))) {
        cat(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==")
        cat("  No need for mice. This data set is completely observed.\n")
        cat(" \\  \\|/  /\n  `-----'\n\n")
      } else {
        pat <- as.numeric(!is.na(data[,c(varlist)]))
        sortpat <-pat[order(pat)]
        mpat <- sortpat[!duplicated(sortpat)]
        mpat2 <- rbind(mpat[1],mpat[2])
        rownames(mpat2) <- table(sortpat)
        colnames(mpat2) <- c(varlist)
        mdtab <- list(mpat2)
      }
    } else {
      mdtab <- list(mice::md.pattern(data[,c(varlist)],plot=plot))
    }
  } else {
    if (length(varlist)==1){
      stop("\n\nThe total number of variables listed in 'y' and 'covs' must be > 1 when using 'by'\n\n",
           call.=TRUE)
      } else {
      bylist <- unlist(strsplit(by," "))
      mdtab <- by(data[,c(varlist)],data[,c(bylist)],
                  function(x) mice::md.pattern(x,plot=plot))
      names(dimnames(mdtab)) = bylist
      }
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
    if (length(varlist)==1){
      mdtmp2 <- mdtmp
    } else {
      mdtmp2 <- mdtmp[,c(varlist)]
    }

    #Combine with summary variables
    mdtmp3 <- cbind(pattern,mdtmp2,n,pct)
    row.names(mdtmp3) <- NULL

    if (length(varlist)==1){
      mdtab[[i]] <- mdtmp3
    } else {
      mdtab[[i]] <- mdtmp3[1:nrow(mdtmp)-1,]
    }
  }

  return(mdtab)
    #result<-data.frame(pattern,mdtmp2,n,pct)
    #row.names(result) <- NULL
    #return(result)
}





