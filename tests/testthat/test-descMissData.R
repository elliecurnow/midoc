# Check output when data are missing
res1<-evaluate_promise(descMissData(y="bmi7", covs="matage mated", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("descMissData output is as expected when data are missing",
  {
    expect_equal(trimws(paste0(gsub("\n","",res1$result), collapse=" "),"right"),
"c(1, 2, 1, 0, 1, 1, 1, 1, 592, 408, 59, 41)")
  }
)

# Check output when by is specified
res2<-evaluate_promise(descMissData(y="bmi7", covs="matage mated", by="pregsize",data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("descMissData output is as expected when 'by' is specified",
          {
            expect_equal(trimws(paste0(gsub("\n","",res2$result), collapse=" "),"right"),
"c(1, 2, 1, 0, 1, 1, 1, 1, 524, 365, 59, 41) c(1, 2, 1, 0, 1, 1, 1, 1, 68, 43, 61, 39)")
          }
)
