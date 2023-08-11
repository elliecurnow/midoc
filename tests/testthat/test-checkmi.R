res1<-evaluate_promise(checkmi(dep="iq8", preds="edscore14 smoking", r="r", mdag="smoking -> iq8 -> edscore14 smoking -> edscore14 smoking -> r"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkmi correctly identifies when MI is valid given the mDAG and imputation model", {
  expect_equal(trimws(paste0(gsub("\n"," ",res1$output), collapse=" "),"right"),
"The incomplete variable and its missingness indicator are independent given imputation model predictors. Hence, multiple imputation methods which assume data are missing at random are valid in principle.")
})



