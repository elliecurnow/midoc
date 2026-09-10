#'Summarise the data distribution for complete and incomplete records
#'
#'Produces a table of descriptive statistics for the analysis model outcome
#'('y') and covariate ('covs') variables, stratified by the complete record
#'indicator ('r_cra') and optionally, by other stratification variable(s).
#'Alternatively, 'y' can indicate the primary variable of interest, e.g. 'y'
#'could refer to an exposure or intervention, with all other variables listed in
#''covs'. The table displays the percentage of missing values for each variable,
#'plus the mean and standard deviation of numeric variables, and frequency and
#'percentage of categorical variables. Optional plots display box plots for
#'numeric variables and stacked bar charts for categorical variables.
#'
#'The summary table is created using the \link[table1]{table1} function. Further
#'arguments can be passed to this function - see the help file for
#'\link[table1]{table1} for more information. Note that the 'transpose' feature
#'is not available. The summary table and plots can be used to compare the
#'characteristics of complete and incomplete records. Note that there may be
#'differences in variable distributions between complete and incomplete records
#'if data are missing at random or missing not at random. Therefore, any
#'observed differences cannot be used as evidence for or against the data
#'missing at random assumption.
#'
#'@param y The analysis model outcome variable(s), specified as a string (space
#'  delimited) or a list
#'@param r_cra The complete record indicator variable, which must be a factor
#'@param data A data frame containing the specified analysis model outcome,
#'  covariate(s), and if specified, stratification variable(s)
#'@param covs Optional analysis model covariate(s), specified as a string (space
#'  delimited) or a list
#'@param by Optional additional stratification variable(s); if specified, the
#'  data are subsetted according to both the complete record indicator and the
#'  values of the stratification variable(s) and summary data are displayed for
#'  each subset in turn
#'@param plot If TRUE (the default), summary plots are displayed; note that
#'  stratification variables are ignored in the plot; use plot = FALSE to
#'  disable the plots
#'@param plotprompt If TRUE (the default), the user is prompted before each plot
#'  is displayed; use plotprompt=FALSE to remove the prompt
#'@param message If TRUE (the default), displays a table of descriptive
#'  statistics comparing complete and incomplete records; use message = FALSE to
#'  suppress the message
#'@param ... Further arguments passed to \link[table1]{table1}
#'
#'@return A summary of descriptive statistics comparing complete and incomplete
#'  records and, optionally, descriptive plots.
#'@export
#'
#' @examples
#' summMissData(y="bmi7", r_cra="r", data=bmi, covs="matage mated bwt pregsize")

summMissData <- function(y, r_cra, data, covs=NULL, by=NULL, plot=TRUE,
                         plotprompt=TRUE, message=TRUE, ...){

  #Error if 'transpose' feature is requested
  args<-list(...)
  if (grepl("transpose", list(args))){
    stop("The 'transpose' option is not available. Please remove this option and try again.")
  }

  #Error if user does not input their dataset or variables
  if (missing(data) || missing(y)) {
    stop("Please provide a dataset and outcome variable.")
  }

  #Error if r_cra is not a factor variable
  if (!is.factor(data[[r_cra]])) {
    #Reminds user they can create r_cra using createMissInd
    stop("Please ensure the complete record indicator, 'r_cra', is a factor variable.")
  }

  #Create summary table
  if(is.null(covs)){
    varlist <- unlist(strsplit(y," "))
  } else {
    varlist <- c(unlist(strsplit(y," ")), unlist(strsplit(covs," ")))
  }

  #Define continuous variable format
  my.render.cont <- function(x) {
    with(table1::stats.apply.rounding(table1::stats.default(x), digits = 2),
         c("", "Mean (SD)" = sprintf("%s (%s)", MEAN, SD)))
  }

  #Define categorical variable format
  my.render.cat <- function(x) {
    c("", sapply(table1::stats.default(x), function(y) with(y,
        sprintf("%d (%0.0f%%)", FREQ, PCT))))
  }

  #Create summary table
  formula <- stats::as.formula(paste("~", paste(c(varlist), collapse = " + "), "|", r_cra))

  if(is.null(by)){
  tab <- as.data.frame(table1::table1(
      formula,
      data = data,
      overall = c("Overall"),
      render.continuous = my.render.cont,
      render.categorical = my.render.cat,
      ...
    ))
  } else {
    bylist <- unlist(strsplit(by," "))
    tab <- by(data[,c(varlist,r_cra)],data[,c(bylist)],
                function(x) as.data.frame(table1::table1(
                  formula,
                  data = x,
                  overall = c("Overall"),
                  render.continuous = my.render.cont,
                  render.categorical = my.render.cat,
                  ...
                )))
    names(dimnames(tab)) = bylist
  }
  tabf <- paste0(gsub(" ", "@",utils::capture.output(print(tab, row.names=FALSE))),prefix="\n",collapse = "\n")

  #Return message
  if(message) {message(paste(gsub("@", " ",strwrap(tabf)),collapse="\n"))}

  if (plot) {
      oask <- grDevices::devAskNewPage(plotprompt)
      for (var in varlist) {
        #if (!var %in% names(data)) next
        #print(var)
        #For factor variables, create stacked bar charts
        if (is.factor(data[[var]])) {
          #Factor plots formation - stacked bar chart
          print(ggplot2::ggplot(data, ggplot2::aes(x = .data[[r_cra]])) +
            ggplot2::geom_bar(ggplot2::aes(fill = .data[[var]]),position="fill", na.rm=TRUE) +
            ggplot2::labs(y = "Proportion", x = "Complete record indicator") +
            ggplot2::ggtitle(paste("Distribution of", var, "by complete record indicator \nbased on all observed data (not stratified)")) +
            ggplot2::theme_minimal() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))

        } else { #Numeric plots formation - boxplot
          print(ggplot2::ggplot(data, ggplot2::aes(x = .data[[r_cra]], y = .data[[var]],  fill = .data[[r_cra]])) +
            ggplot2::geom_boxplot(alpha = 0.5, na.rm = TRUE) +
            ggplot2::labs(y = var, x = "Complete record indicator") +
            ggplot2::ggtitle(paste("Observed distribution of", var, "by complete record indicator \nbased on all observed data (not stratified)")) +
            ggplot2::theme_minimal() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
            ggplot2::theme(legend.position = "none"))
        }
      }
      #Reset original settings
      grDevices::devAskNewPage(oask)
    }

  #Return summary table
  invisible(tab)
}
