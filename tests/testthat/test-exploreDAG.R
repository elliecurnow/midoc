# Check output when there are no testable paths
res1<-evaluate_promise(exploreDAG(mdag="matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated bmi7 -> r", data=bmi))
#Trim output for test purposes
test_that("exploreDAG correctly identifies both the implied independencies and that
          none of them are testable",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),
                               "right"),497,745),
"None of the fully observed variables are conditionally independent. Hence, no consistency checks will be performed.  Consider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g")
  }
)

# Check output when there are testable paths
res2<-evaluate_promise(exploreDAG(mdag="matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated sep_unmeas -> r", data=bmi))
#Trim output for test purposes
test_that("exploreDAG correctly identifies both the implied independencies and
          the testable subset",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res2$messages), collapse=" "),
                               "right"),517,682),
"These (conditional) independence statements are explored below using the canonical correlations approach for mixed data. See ??dagitty::localTests for further details")
  }
)

# Check output when a dataset is not supplied
res3<-evaluate_promise(exploreDAG(mdag="matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated bmi7 -> r"))
#Trim output for test purposes
test_that("exploreDAG correctly identifies the implied independencies",
          {
            expect_equal(substr(trimws(paste0(gsub("\n"," ",res3$messages), collapse=" "),
                                       "right"),1,334),
                         "The proposed directed acyclic graph (DAG) implies the following pairs of variables are (conditionally) independent (where, for example, 'X _||_ Y | Z' should be read as 'X is independent of Y conditional on Z'). Note that variable names are abbreviated. Consider whether these (conditional) independencies are plausible for your study")
          }
)

# Check output when a dagitty object is supplied
dag <- dagitty::dagitty("matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated bmi7 -> r", layout=T)
res4<-evaluate_promise(exploreDAG(mdag=dag))
#Trim output for test purposes
test_that("exploreDAG correctly identifies the implied independencies when a dagitty object is supplied",
          {
            expect_equal(substr(trimws(paste0(gsub("\n"," ",res4$messages), collapse=" "),
                                       "right"),1,334),
                         "The proposed directed acyclic graph (DAG) implies the following pairs of variables are (conditionally) independent (where, for example, 'X _||_ Y | Z' should be read as 'X is independent of Y conditional on Z'). Note that variable names are abbreviated. Consider whether these (conditional) independencies are plausible for your study")
          }
)
