# First specify each imputation model as a 'mimod' object, suppressing the
## message
mimod_bmi7 <- checkModSpec(
  formula="bmi7~matage+I(matage^2)+mated+pregsize",
  family="gaussian(identity)", data=bmi, message=FALSE)
mimod_pregsize <- checkModSpec(
  formula="pregsize~bmi7+matage+I(matage^2)+mated",
  family="binomial(logit)", data=bmi, message=FALSE)
# Check the proposed 'mice' options when specifying more than one imputation
## model (suppressing the plot)
res1<-evaluate_promise(proposeMI(mimodobj=list(mimod_bmi7,mimod_pregsize),
                                 data=bmi, plot = FALSE))
#Trim output for test purposes
test_that("proposeMI suggests correct mice options and creates expected object",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),
                               "right"),1,110),
"Based on your proposed imputation model and dataset, your mice() call should be as follows:  mice(data = bmi ,")
    expect_equal(res1$result$m,41)
    expect_equal(res1$result$method,list("norm", "logreg"))
    expect_equal(paste0(res1$result$formulas),c("bmi7 ~ matage + I(matage^2) + mated + pregsize", "pregsize ~ bmi7 + matage + I(matage^2) + mated"))
  }
)

# Check the proposed 'mice' options when no dataset is specified
mimod <- checkModSpec(
  formula="bmi7~matage+I(matage^2)+mated+pregsize",
  family="gaussian(identity)", message=FALSE)
#res2<-evaluate_promise(proposeMI(mimodobj=mimod))
#Trim output for test purposes
test_that("proposeMI gives a warning if neither data nor prop_complete are specified",
          {
            expect_error(proposeMI(mimodobj=mimod),
                         "'prop_complete' must be specified, or else 'data' must be specified")
          }
)

res3<-evaluate_promise(proposeMI(mimodobj=mimod, prop_complete=0.5))
#Trim output for test purposes
test_that("proposeMI suggests correct mice options and creates expected object if dataset not specified",
          {
            expect_equal(substr(trimws(paste0(gsub("\n"," ",res3$messages), collapse=" "),
                                       "right"),1,185),
                         "Based on your proposed imputation model and dataset, your mice() call should be as follows:  mice(data = NULL , # You may need to specify a subset of the columns in your dataset  m = 50")
          }
)

bmi2 <- bmi
res4<-evaluate_promise(proposeMI(mimodobj=mimod_bmi7, data=bmi2, plot=FALSE, message=FALSE))
test_that("proposeMI gives a warning if midmodobj dataset does not match specified dataset",
          {
            expect_equal(trimws(res4$warnings),
                         "The names of the datasets used to specify the set of imputation models do not match the dataset provided. Check that the specification of each imputation model was explored using the same dataset.")
          }
)
