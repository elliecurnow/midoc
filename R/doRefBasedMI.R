#' Performs reference-based multiple imputation
#'
#' Creates multiple imputations using \link[RefBasedMI]{RefBasedMI}, based on
#' the dataset and relevant options specified by a call to
#' \link[midoc]{proposeMI}. If a substantive model is specified, also calculates
#' the pooled estimates using \link[mice]{pool}.
#'
#' The dataset is assumed to be in 'wide' format. Data are assumed to be
#' multivariate normal within each treatment arm. See
#' \link[RefBasedMI]{RefBasedMI} for further details.
#'
#' @param mipropobj An object of type 'miprop', created by a call to 'proposeMI'
#' @param covs The analysis model covariate(s), specified as a string (space
#'   delimited)
#' @param depvar The longitudinal outcome variable(s), specified as a string
#'   (space delimited)
#' @param treatvar Numeric treatment group variable; values must be positive
#'   integers
#' @param idvar Participant identifier variable
#' @param method Reference-based imputation method; methods that are
#'   supported are "J2R", "CR", and "CIR"
#' @param reference Numeric reference group for the specified method
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
#' # First specify the imputation model as a 'mimod' object
#' ## (suppressing the message)
#' mimod_qol12 <- checkModSpec(formula="qol12 ~ group + age0 + qol0 + qol3",
#'                            family="gaussian(identity)",
#'                            data=qol,
#'                            message=FALSE)
#' # Save the proposed 'mice' options as a 'miprop' object
#' ## (suppressing the message)
#' miprop_qol12 <- proposeMI(mimodobj=mimod_qol12,
#'                     data=qol,
#'                     message=FALSE,
#'                     plot = FALSE)
#' # Create the set of imputed datasets using the proposed 'mice' options and
#' ## specified reference-based imputation method; then, fit the substantive
#' ## model to each imputed dataset and display the pooled results
#' doRefBasedMI(mipropobj=miprop_qol12, covs=c("age0","qol0"),
#'              depvar=c("qol3", "qol12"), treatvar="group",
#'              idvar="id", method="J2R", reference=1, seed=123,
#'              substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")
doRefBasedMI <- function(mipropobj, covs, depvar, treatvar, idvar, method,
                         reference, seed, substmod = " ", message = TRUE) {

  # Coerce reference group to value of 0
  treatgrp <- vector()
  for(i in 1:nrow(mipropobj$data)){
    treatgrp[i] <- ifelse(mipropobj$data[,treatvar][i] == reference, 0, mipropobj$data[,treatvar][i])
  }

  # Arrange dataset in 'long' format
  data_long <- data.frame()
  for (i in 1:length(depvar)){

    #Add depvar_name to the dataset
    data_long <- base::rbind(data_long,
                             base::cbind(mipropobj$data[,covs],
                                         id=mipropobj$data[,idvar],
                                         treatgrp=treatgrp,
                                         y=mipropobj$data[,depvar[i]],
                                         time=i))
  }
  # Rename baseline covariates
  for(i in 1:length(covs)){
    names(data_long)[i]=paste("covar",i,sep="")
  }

  # Sort by id
  data_long <- data_long[order(data_long$id),]

  # Run RefBasedMI
  # Using if statements to overcome problem with specifying method and covars in terms of fn parameters
  if (method=="J2R"){
    if (length(covs)==1){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=covar1,
                              treatvar=treatgrp, idvar=id,
                              timevar=time, method="J2R", reference=0,
                              M=mipropobj$m,
                              seed=seed))
    } else if (length(covs)==2){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=c(covar1,covar2),
                                             treatvar=treatgrp, idvar=id,
                                             timevar=time, method="J2R", reference=0,
                                             M=mipropobj$m,
                                             seed=seed))
    } else {stop('A maximum of two baseline covariates are allowed')}
  } else if (method=="CR"){
    if (length(covs)==1){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=covar1,
                                           treatvar=treatgrp, idvar=id,
                                           timevar=time, method="CR", reference=0,
                                           M=mipropobj$m,
                                           seed=seed))
    } else if (length(covs)==2){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=c(covar1,covar2),
                                           treatvar=treatgrp, idvar=id,
                                           timevar=time, method="CR", reference=0,
                                           M=mipropobj$m,
                                           seed=seed))
    } else {stop('A maximum of two baseline covariates are allowed')}
  } else if (method=="CIR"){
    if (length(covs)==1){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=covar1,
                                           treatvar=treatgrp, idvar=id,
                                           timevar=time, method="CIR", reference=0,
                                           M=mipropobj$m,
                                           seed=seed))
    } else if (length(covs)==2){
      refbasedmi <- suppressMessages(RefBasedMI::RefBasedMI(data=data_long, depvar=y, covar=c(covar1,covar2),
                                           treatvar=treatgrp, idvar=id,
                                           timevar=time, method="CIR", reference=0,
                                           M=mipropobj$m,
                                           seed=seed))
    } else {stop('A maximum of two baseline covariates are allowed')}
  } else {stop('Method must be one of "J2R", "CR", or "CIR"')}


  # Return names to original names and re-format in original 'wide' form
  names(refbasedmi)[names(refbasedmi) == "treatgrp"] <- treatvar
  names(refbasedmi)[names(refbasedmi) == "id"] <- idvar
  names(refbasedmi)[names(refbasedmi) == "covar1"] <- covs[1]
  names(refbasedmi)[names(refbasedmi) == "covar2"] <- covs[2]

  refbasedmi_wide <- data.frame()

  for (i in 1:length(depvar)){

    #Add depvar_name to the dataset
    tmp <- subset(refbasedmi, time==i, c(covs, "y", treatvar, idvar, ".imp"))
    names(tmp)[names(tmp) == "y"] <- depvar[i]
    if (i==1) refbasedmi_wide <- tmp
    else refbasedmi_wide <- merge(refbasedmi_wide, tmp)
  }

  # Sort by .imp
  refbasedmi_wide <- refbasedmi_wide[order(refbasedmi_wide$.imp),]

  # Coerce to mids
  refbasedmi_wide_mids <- mice::as.mids(refbasedmi_wide)

  #If a substantive model is specified, calculate the pooled estimates
  if(substmod != " "){
    mipo <- mice::pool(with(refbasedmi_wide_mids,parse(text=substmod, keep.source=FALSE)))
    result <- paste("Given the substantive model:",
                    substmod,
                    "using reference-based imputation method:",
                    method,
"\n, multiple imputation estimates are as follows: \n \n",
              paste0(gsub(" ", "@",utils::capture.output(summary(mipo,conf.int=TRUE))),prefix="\n",collapse = "\n"),
              collapse = "\n")
  }
  else {
    result <- paste("Now you have created your multiply imputed datasets, you can perform your analysis and pool the results using the 'mice' functions 'with()' and 'pool()'", collapse = "\n")
    }

  if(message) {message(paste(gsub("@", " ",strwrap(result)),collapse="\n"))}

  invisible(refbasedmi_wide_mids)

}
