# First specify the imputation model as a 'mimod' object, suppressing the
## message
mimod_qol12 <- checkModSpec(formula="qol12 ~ group + age0 + qol0 + qol3",
                           family="gaussian(identity)",
                           data=qol,
                           message=FALSE)
# Save the proposed 'mice' options as a 'miprop' object, suppressing the
## message and plots
miprop_rbi <- proposeMI(mimodobj=mimod_qol12, data=qol, plot=FALSE, message=FALSE)
# Check both the output when a substantive model is specified and that a
## mice object is created
res1<-evaluate_promise(doRefBasedMI(mipropobj=miprop_rbi, y="qol3 qol12",
                                   groupvar="group", idvar="id", method="J2R", reference=1, seed=123,
                                  substmod = "lm(qol12 ~ factor(group))"))
#Trim output for test purposes
test_that("doRefBasedMI creates both the correct output when a substantive model is
         specified and a mice 'mids' object",
{
 expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$message), collapse=" "),
                           "right"),1,101),
"Given the substantive model: lm(qol12 ~ factor(group)) using reference-based imputation method: J2R w")
   expect_equal(mice::is.mids(res1$result),TRUE)
}
)

# Check the output when a substantive model is not specified
res2<-evaluate_promise(doRefBasedMI(mipropobj=miprop_rbi, y="qol3 qol12",
                                   groupvar="group", idvar="id", method="J2R", reference=1, seed=123))
#Trim output for test purposes
test_that("doRefBasedMI creates the correct output when a substantive model is
         not specified",
        {
         expect_equal(substr(trimws(paste0(gsub("\n"," ",res2$message), collapse=" "),
                                   "right"),1,101),
                    "Now you have created your multiply imputed datasets, you can perform your analysis and pool the resul")
      expect_equal(mice::is.mids(res2$result),TRUE)
   }
)

# Check the output when covs and id are not specified
res3<-evaluate_promise(doRefBasedMI(mipropobj=miprop_rbi, y="qol3 qol12",
                                   groupvar="group", method="J2R", reference=1, seed=123))
#Trim output for test purposes
test_that("doRefBasedMI creates the correct output when covs and id are
         not specified",
        {
         expect_equal(substr(trimws(paste0(gsub("\n"," ",res3$message), collapse=" "),
                                   "right"),1,101),
                    "Now you have created your multiply imputed datasets, you can perform your analysis and pool the resul")
      expect_equal(mice::is.mids(res3$result),TRUE)
   }
)

# Check a single outcome variable is rejected with an informative error
test_that("doRefBasedMI gives an informative error for a single outcome variable",
          {
            expect_error(doRefBasedMI(mipropobj=miprop_rbi, y="qol12", groupvar="group",
                                      method="J2R", reference=1, seed=123),
                         "At least two longitudinal outcome variables")
          }
)

# Check an incorrect method is rejected with an informative error
test_that("doRefBasedMI gives an informative error for an incorrect method",
          {
            expect_error(doRefBasedMI(mipropobj=miprop_rbi, y="qol3 qol12", groupvar="group",
                                      method="LOCF", reference=1, seed=123),
                         'Method must be one of "J2R", "CR", or "CIR"')
          }
)

# Check >5 covs is rejected with an informative error
test_that("doRefBasedMI gives an informative error for > 5 covs",
          {
            expect_error(doRefBasedMI(mipropobj=miprop_rbi, y="qol3 qol12",
                                      covs="age0 qol0 id group qol3 r",
                                      groupvar="group",
                                      method="J2R", reference=1, seed=123),
                         'A maximum of five baseline covariates are allowed; you will need to use a direct call to the RefBasedMI function if you want to include more than five baseline covariates')
          }
)
