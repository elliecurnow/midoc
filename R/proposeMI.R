#' Suggests multiple imputation options
#'
#' Suggests the optimal \link[mice]{mice} options to perform multiple
#' imputation, based on the proposed imputation model and specified dataset.
#'
#' @param mimodobj An object of type 'mimod', which stands for 'multiple
#'   imputation model', created by a call to \link[midoc]{checkModSpec}
#' @param data A data frame containing all the variables required for the
#'   imputation and substantive analysis models
#' @param diagplot If TRUE (the default), displays diagnostic plots for the
#'   proposed 'mice' call; use diagplot=FALSE to disable the plots
#'
#' @return A message describing the proposed 'mice' options, plus an object of
#'  type 'miprop', which can be used to run 'mice' using the proposed options
#'
#' @export
#'
#' @examples
#' mimod <- checkModSpec(formula=bmi7~matage+I(matage^2)+mated+pregsize,
#' family=gaussian(identity), data=bmi)
#' proposeMI(mimod, data=bmi)
proposeMI <- function(mimodobj, data, diagplot = TRUE) {

  m_min <- ceiling((1-mean(ifelse(apply(data,1,anyNA)==F,1,0)))*100)

  if(mimodobj$family == "gaussian(identity)") {method <- "norm"}
  else if(mimodobj$family == "binomial(logit)"){method <- "logreg"}

  formulas_list <- as.list(c(stats::as.formula(paste(mimodobj$formula,collapse=" "))))

#Create object which is as below
#summary(proposeMI.object) gives you this message:
cat(strwrap("Based on your proposed imputation model and dataset, your mice() call should be as follows:"),
"\nmice(data = ", mimodobj$datalab,",",
strwrap("# You may need to specify a subset of the columns in your dataset"),"\n",
"\nm = ", m_min, ",", strwrap("# You should use at least this number of imputations
based on the proportion of complete records in your dataset"),"\n",
"\nmethod = ", method,",",
strwrap("# Specify a method for each incomplete variable. If displayed, the box-and-whisker plots can be used to inform
your choice of method(s): for example, if the imputation model does not predict extreme values appropriately,
consider a different imputation model/method e.g. PMM.
Note the distribution of imputed and observed values may differ if data are missing at random.
If you suspect data are missing not at random, the plots can also inform your choice of sensitivity parameter"),"\n",
strwrap("\nformulas = formulas_list , # Note that you do not additionally need to specify a 'predmatrix'"),"\n",
strwrap("The conditional imputation models are:"),"\n",
paste(mimodobj$formula,collapse=" "),"\n",
strwrap("\nmaxit = 10 , # If you have more than one incomplete variable, you should check
this number of iterations is sufficient by inspecting the trace plots, if displayed.
Consider increasing the number of iterations if there is a trend that does not stabilise by the 10th iteration.
Note that iteration is not required when only one variable is incomplete."),"\n",
strwrap("\nprintFlag = FALSE , # Change to print=TRUE to display the history as imputation is performed"),"\n",
strwrap("\nseed = NA) # It is good practice to choose a seed so your results are reproducible"),"\n",fill=TRUE)

#Optionally create a plot of observed versus imputed values and a trace plot for five imputed datasets
if (diagplot){
  imp <- mice::mice(data = data, method = method,
                    formulas = formulas_list, maxit = 20, printFlag = FALSE)
  #changed from bwplot to densityplot
  print(mice::bwplot(imp,
        main=list("Plot of imputed (red) values, with distribution of \nobserved (blue) values for comparison",cex=0.9)))

  #Prompt for second plot after first plot is displayed
  oask <- grDevices::devAskNewPage(TRUE)
  print(plot(imp,main=list("Trace plots across 20 iterations",cex=0.9)))
  #Reset original settings
  grDevices::devAskNewPage(oask)
}

#Output an object with the proposed specification
miprop <- list(data=data,m= m_min,method=method,formulas = formulas_list)
invisible(miprop)
}