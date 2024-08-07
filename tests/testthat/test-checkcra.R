#Check output when CRA is valid
res1<-evaluate_promise(checkCRA(y="edscore14", covs="iq8", r="r", mdag="iq8 -> edscore14 iq8 -> r"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkCRA correctly identifies when CRA is valid given the mDAG and analysis model", {
  expect_equal(trimws(paste0(gsub("\n","",res1$output), collapse=" "),"right"),
  "Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are independent given analysis model covariates. Hence, complete records analysis is valid.")
  }
  )

#Check output when CRA is not valid
#res2<-evaluate_promise(checkCRA(y="edscore14", covs=" ", r="r", mdag="iq8 -> edscore14 iq8 -> r"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
#test_that("checkCRA correctly identifies when CRA is valid given the mDAG and analysis model", {
#  expect_equal(trimws(paste0(gsub("\n","",res1$output), collapse=" "),"right"),
#               "Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are independent given analysis model covariates. Hence, complete records analysis is valid.")
#}
#)


