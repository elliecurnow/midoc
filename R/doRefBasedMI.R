#' Performs reference-based multiple imputation
#'
#' Creates multiple imputations under reference-based multiple imputation using
#' \link[RefBasedMI]{RefBasedMI}. Imputations are based on the dataset and
#' relevant options specified by a call to \link[midoc]{proposeMI}. If a
#' substantive model is specified, the pooled estimates are also calculated
#' using \link[mice]{pool}. The dataset is assumed to be in 'wide' format, with
#' one row per subject. It is assumed that the outcome is measured repeatedly
#' over time, with at least one measurement at an intermediate time-point (i.e.
#' between baseline and study end-point). Data are assumed to be multivariate
#' normal within each category of the grouping variable (in typical use, this
#' denotes the treatment allocation).
#'
#' Reference-based multiple imputation uses observed data from one category of
#' the grouping variable - the 'reference' group - to impute missing values in
#' other categories. Available reference-based methods are 'jump-to-reference'
#' (J2R), 'copy reference' (CR), and 'copy increments in reference' (CIR). J2R
#' assumes that the distribution of outcomes for individuals who drop out 'jumps
#' to' the distribution observed in the reference group following their last
#' observed time point. CR assumes individuals who drop out behave as if they
#' are in the specified reference group for the full duration of the trial. CIR
#' assumes that the distribution of outcomes for individuals who drop out
#' follows the mean increments observed in the reference group, following their
#' last observed time point.
#'
#' @param mipropobj An object of type 'miprop', created by a call to 'proposeMI'
#' @param y The analysis model outcome variables (at least two are required),
#'   specified as a string (space delimited) or a list
#' @param covs Optional analysis model covariate(s), specified as a string
#'   (space delimited) or a list; a maximum of five covariates can be specified
#' @param groupvar Group variable; can be numeric or string
#' @param idvar Optional participant identifier variable; if not provided, an
#'   identifier variable, named 'id', will be automatically created
#' @param method Reference-based imputation method; methods that are supported
#'   are "J2R", "CR", and "CIR"
#' @param reference Reference group for the specified method; can be numeric or
#'   string
#' @param seed An integer that is used to set the seed of the 'mice' call
#' @param substmod Optionally, a symbolic description of the substantive model
#'   to be fitted, specified as a string; if supplied, the model will be fitted
#'   to each imputed dataset and the results pooled
#' @param message If TRUE (the default), displays a message summarising the
#'   analysis that has been performed; use message = FALSE to suppress the
#'   message
#'
#' @return A 'mice' object of class 'mids' (the multiply imputed datasets).
#'   Optionally, a message summarising the analysis that has been performed.
#'
#' @export
#'
#' @examplesIf interactive()
#' # First specify the imputation model as a 'mimod'object
#' ## (suppressing the message)
#' mimod_qol12 <- checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 +
#'   qol3", family="gaussian(identity)", data=qol, message=FALSE)
#'
#' # Save the proposed 'mice' options as a 'miprop' object
#' ## (suppressing the message)
#' miprop_qol12 <- proposeMI(mimodobj=mimod_qol12, data=qol, message=FALSE,
#'   plot = FALSE)
#'
#' # Create the set of imputed datasets using the proposed mice' options and
#' ## specified reference-based imputation method; then fit the substantive
#' ## model to each imputed dataset and display the pooled results
#' doRefBasedMI(mipropobj=miprop_qol12, y="qol3 qol12", groupvar="group",
#'  covs="age0 qol0", idvar="id", method="J2R", reference=1, seed=123,
#'  substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")
doRefBasedMI <- function(mipropobj, y, groupvar, covs = NULL, idvar = NULL, method,
                         reference, seed, substmod = NULL, message = TRUE) {

  # Check one of the reference-based methods is specified
  if (!method %in% c("J2R", "CR", "CIR")) {
    stop('Method must be one of "J2R", "CR", or "CIR"')
  }

  # Vectorise variable strings
  ylist <- unlist(strsplit(y," "))
  if (length(ylist) < 2) {
    stop("At least two longitudinal outcome variables must be specified in 'y'")
  }
  yvar <- mipropobj$data[ , ylist, drop=FALSE]

  covslist <- NULL
  if(!is.null(covs)){
    covslist <- unlist(strsplit(covs," "))
    if (length(covslist) > 5) {
      stop('A maximum of five baseline covariates are allowed; you will need to use a direct call to the RefBasedMI function if you want to include more than five baseline covariates')
    }
    #covar <- mipropobj$data[ , covslist, drop=FALSE]
    for (i in seq_along(covslist)){
      expr <- paste("covar",i, "<- mipropobj$data[ , covslist[i], drop=FALSE]",sep="")
      eval(parse(text=expr))
    }
  }

  #Define variables required for RefBasedMI within fn to avoid global variable error
  if(!is.null(idvar)){
    id <- mipropobj$data[,idvar]
  } else {
    id=c(1:nrow(mipropobj$data))
  }

  time <- seq_along(ylist)

  # Arrange dataset in 'long' format
  data_long <- data.frame()
  for (i in seq_along(ylist)){
    data_long <- base::rbind(data_long,
                             base::cbind(mipropobj$data[, covslist, drop=FALSE],
                                         id=id,
                                         group=mipropobj$data[,groupvar, drop=FALSE],
                                         yvar=yvar[,i],
                                         time=c(rep(time[i],nrow(mipropobj$data)))))
  }

  # Rename baseline covariates in data_long
  for(i in seq_along(covslist)){
    names(data_long)[i]=paste("covar",i,sep="")
  }

  # Sort by id
  data_long <- data_long[order(data_long$id),]

  # RefBasedMI deparses its method and covar arguments, so splice their values
  # into the call as literals rather than passing them as variables
  # There seems to be no solution other than the literal coding below when using RefBasedMI with multiple covars
  covsexpr <- NULL
  if(!is.null(covs)){
    covsexpr <- if (length(covslist)==1) quote(covar1) else
                  if (length(covslist)==2) (quote(c(covar1,covar2))) else
                    if (length(covslist)==3) quote(c(covar1,covar2,covar3)) else
                      if (length(covslist)==4) quote(c(covar1,covar2,covar3,covar4)) else
                        quote(c(covar1,covar2,covar3,covar4,covar5))
  }
  #covsexpr <- paste("c(", paste(covslist, collapse=","), ")")

  refbasedmi <- suppressMessages(eval(bquote(
    RefBasedMI::RefBasedMI(data=data_long, depvar=yvar, covar=.(covsexpr),
                           treatvar=group, idvar=id,
                           timevar=time, method=.(method), reference=.(reference),
                           M=mipropobj$m,
                           seed=seed))))

    # Return names to original names and re-format in original 'wide' form
  names(refbasedmi)[names(refbasedmi) == "group"] <- groupvar
  names(refbasedmi)[names(refbasedmi) == "covar1"] <- covslist[1]
  names(refbasedmi)[names(refbasedmi) == "covar2"] <- covslist[2]
  names(refbasedmi)[names(refbasedmi) == "covar3"] <- covslist[3]
  names(refbasedmi)[names(refbasedmi) == "covar4"] <- covslist[4]
  names(refbasedmi)[names(refbasedmi) == "covar5"] <- covslist[5]

  refbasedmi_wide <- data.frame()

  for (i in seq_along(ylist)){

    #Add ylist names to the dataset
    tmp <- subset(refbasedmi, time==i, c(covslist, "yvar", groupvar, "id", ".imp"))
    names(tmp)[names(tmp) == "yvar"] <- ylist[i]
    if (i==1) refbasedmi_wide <- tmp else
      refbasedmi_wide <- merge(refbasedmi_wide, tmp)
  }

  if(!is.null(idvar)) names(refbasedmi_wide)[names(refbasedmi_wide) == "id"] <- idvar

  # Sort by .imp
  refbasedmi_wide <- refbasedmi_wide[order(refbasedmi_wide$.imp),]

  # Coerce to mids
  refbasedmi_wide_mids <- mice::as.mids(refbasedmi_wide)

  #If a substantive model is specified, calculate the pooled estimates
  if(!is.null(substmod)){
    mipo <- mice::pool(with(refbasedmi_wide_mids,parse(text=substmod, keep.source=FALSE)))
    result <- paste("Given the substantive model:",
                    substmod,
                    "using reference-based imputation method:",
                    method,
                    "with reference = ",
                    reference,
"\n, multiple imputation estimates are as follows: \n \n",
              paste0(gsub(" ", "@",utils::capture.output(summary(mipo,conf.int=TRUE))),"\n",collapse = "\n"),
              collapse = "\n")
  }
  else {
    result <- paste("Now you have created your multiply imputed datasets, you can perform your analysis and pool the results using the 'mice' functions 'with()' and 'pool()'", collapse = "\n")
    }

  if(message) {message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))}

  invisible(refbasedmi_wide_mids)

}
