#Check error message occurs when transpose option is used
test_that("Error occurs if transpose option is used", {
  expect_error(summMissData(r_cra = "r_cra", covs = c("age"), transpose=T),
               "The 'transpose' option is not available. Please remove this option and try again.")
})

#Check error message occurs when dataset or variables are missing
test_that("Error occurs if data or y are missing", {
  expect_error(summMissData(r_cra = "r_cra", covs = c("age")),
               "Please provide a dataset and outcome variable.")
})

#Check error message occurs when r_cra is not a factor
test_that("Error occurs if r_cra is not a factor", {
  expect_error(summMissData(y="bmi7", r_cra="matage", data=bmi),
               "Please ensure the complete record indicator, 'r_cra', is a factor variable.")
})

#Check function returns a list object
test_that("Function returns a list object", {
  result <- summMissData(y="bmi7", r_cra="r", data=bmi, covs="matage mated bwt pregsize", message = FALSE, plot=FALSE)
  expect_type(result, "list")
})



