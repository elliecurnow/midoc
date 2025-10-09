# First specify the imputation model as a 'mimod' object, suppressing the
## message
mimod_qol12 <- checkModSpec(formula="qol12 ~ group + age0 + qol0 + qol3",
                           family="gaussian(identity)",
                           data=qol,
                           message=FALSE)
# Save the proposed 'mice' options as a 'miprop' object, suppressing the
## message and plots
miprop <- proposeMI(mimodobj=mimod_qol12, data=qol, plot=FALSE, message=FALSE)
# Check both the output when a substantive model is specified and that a
## mice object is created
#res1<-evaluate_promise(doRefBasedMI(miprop, covs=c("age0","qol0"),
#                                    depvar=c("qol3", "qol12"), treatvar="group",
#                                    idvar="id", method="J2R", reference=1, seed=123,
#                        substmod="lm(qol12 ~ factor(group) + age0 + qol0)"))
#Trim output for test purposes
#test_that("doRefBasedMI creates both the correct output when a substantive model is
#          specified and a mice 'mids' object",
#  {
#    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$message), collapse=" "),
#                               "right"),1,101),
#"Given the substantive model: lm(qol12 ~ factor(group) + age0 + qol0) using reference-based imputation")
#    expect_equal(mice::is.mids(res1$result),TRUE)
#  }
#)


