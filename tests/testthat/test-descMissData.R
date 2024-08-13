# Check output when data are missing
res1<-evaluate_promise(descMissData(y="bmi7", covs="matage mated", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("descMissData output is as expected when data are missing",
  {
    expect_equal(trimws(paste0(gsub("\n","",res1$output), collapse=" "),"right"),
" pattern bmi7 matage mated   n pct       1    1      1     1 592  59       2    0      1     1 408  41")
  }
)

