#' Suggests multiple imputation options
#'
#' Suggests the \link[mice]{mice} options to perform multiple
#' imputation, based on the proposed set of imputation models (one for each
#' partially observed variable) and specified dataset.
#'
#' @param mimodobj An object, or list of objects, of type 'mimod', which stands
#'   for 'multiple imputation model', created by a call to
#'   \link[midoc]{checkModSpec}
#' @param data A data frame containing all the variables required for imputation
#'   and the substantive analysis
#' @param message If TRUE (the default), displays a message describing the
#'   proposed 'mice' options; use message=FALSE to suppress the message
#' @param plot If TRUE (the default), displays diagnostic plots for the
#'   proposed 'mice' call; use plot=FALSE to disable the plots
#' @param plotprompt If TRUE (the default), the user is prompted before the
#'   second plot is displayed; use plotprompt=FALSE to remove the prompt
#'
#' @return An object of type 'miprop', which can be used to run 'mice' using the
#' proposed options, plus, optionally, a message and diagnostic plots describing
#' the proposed 'mice' options
#'
#' @export
#'
#' @examples
#' # First specify each imputation model as a 'mimod' object
#' mimod_bmi7 <- checkModSpec(
#' formula="bmi7~matage+I(matage^2)+mated+pregsize",
#'   family="gaussian(identity)", data=bmi)
#' mimod_pregsize <- checkModSpec(
#' formula="pregsize~bmi7+matage+I(matage^2)+mated",
#'   family="binomial(logit)", data=bmi)
#'
#' # Display the proposed 'mice' options
#'   ## When specifying a single imputation model
#' proposeMI(mimodobj=mimod_bmi7, data=bmi)
#'   ## When specifying more than one imputation model
#' proposeMI(mimodobj=list(mimod_bmi7,mimod_pregsize), data=bmi)
proposeMI <- function(mimodobj, data, message = TRUE, plot = TRUE, plotprompt = TRUE) {

  m_min <- ceiling((1-mean(ifelse(apply(data,1,anyNA)==F,1,0)))*100)

  if(max(lengths(mimodobj)==1)){
    mimod_count <- 1
  } else {
    mimod_count <- length(mimodobj)
    }

  #Specify method, formula, dataset name for each variable to be imputed
  method <- vector("list", mimod_count)
  formulas_list <- vector("list", mimod_count)
  #Keep specified dataset name as ref value
  datalab <- deparse(substitute(data))

  for (i in 1:mimod_count){
    if (mimod_count > 1){
        family <- mimodobj[[i]][["family"]]
        formula <- mimodobj[[i]][["formula"]]
        datalab_check <- mimodobj[[i]][["datalab"]]
    } else {
        family <- mimodobj[["family"]]
        formula <- mimodobj[["formula"]]
        datalab_check <- mimodobj[["datalab"]]
    }

    if(family == "gaussian(identity)") {
      method[i] <- "norm"
    } else if(family == "binomial(logit)") {
      method[i] <- "logreg"
      }

    formulas_list[i] <- c(stats::as.formula(paste(formula,collapse=" ")))

    if (datalab_check != datalab){
        warning("The names of the datasets used to specify the set of imputation models do not match the dataset provided.
Check that the specification of each imputation model was explored using the same dataset.", call.=FALSE)
        }
  }

  if (message) {
    cat(strwrap("Based on your proposed imputation model and dataset, your mice() call should be as follows:"),
    "\nmice(data = ", datalab,",",
    strwrap("# You may need to specify a subset of the columns in your dataset"),"\n",
    "\nm = ", m_min, ",", strwrap("# You should use at least this number of imputations
    based on the proportion of complete records in your dataset"),"\n",
    "\nmethod = c(",paste(sQuote(method), collapse=", "),")","\n")
    cat(strwrap("# Specify a method for each incomplete variable. \nIf displayed, the box-and-whisker plots can be used to inform
    your choice of method(s): for example, if the imputation model does not predict extreme values appropriately,
    consider a different imputation model/method e.g. PMM. Note the distribution of
    imputed and observed values is displayed for numeric variables only. The distribution
    may differ if data are missing at random. If you suspect data are missing not at random,
    the plots can also inform your choice of sensitivity parameter."),
    "\n", "\n",
    strwrap("\nformulas = formulas_list , # Note that you do not additionally need to specify a 'predmatrix'"),"\n",
    strwrap("# The formulas_list specifies the conditional imputation models, as follows:"),"\n",
    paste(sQuote(formulas_list), collapse="\n"),"\n",
    strwrap("\nmaxit = 10 , \n# If you have more than one incomplete variable, you should check
    this number of iterations is sufficient by inspecting the trace plots, if displayed.
    Consider increasing the number of iterations if there is a trend that does not
    stabilise by the 10th iteration. Note that iteration is not required when only one variable is
    partially observed."),"\n","\n",
    strwrap("\nprintFlag = FALSE , # Change to printFlag=TRUE
    to display the history as imputation is performed"),"\n",
    strwrap("\nseed = NA) # It is good practice to choose a seed so your results are reproducible"),"\n",fill=TRUE)
  }

  #Optionally create a plot of observed versus imputed values and a trace plot for five imputed datasets
  if (plot){
    imp <- mice::mice(data = data, method = method,
                      formulas = formulas_list, maxit = 20, printFlag = FALSE)
    #changed from bwplot to densityplot
    print(mice::bwplot(imp,
          main=list("Plot of imputed (red) values, with distribution of \nobserved (blue) values for comparison",cex=0.9)))

    #Optionally prompt for second plot after first plot is displayed
    if (plotprompt){
      oask <- grDevices::devAskNewPage(TRUE)
      print(plot(imp,main=list("Trace plots across 20 iterations",cex=0.9)))
      #Reset original settings
      grDevices::devAskNewPage(oask)
    } else {
      print(plot(imp,main=list("Trace plots across 20 iterations",cex=0.9)))
    }
  }

#Output an object with the proposed specification
miprop <- list(data=data,m= m_min,method=method,formulas = formulas_list)
invisible(miprop)
}
