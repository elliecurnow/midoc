# Lists missing data patterns in the specified dataset

This function summarises the missing data patterns in the specified
dataset. Each row in the output corresponds to a missing data pattern
(1=observed, 0=missing). The number and percentage of observations is
also displayed for each missing data pattern. The first column indicates
the number of missing data patterns. The second column refers to the
analysis model outcome ('y'), with all other variables ('covs')
displayed in subsequent columns. Alternatively, 'y' can be used to
display the primary variable of interest, e.g. 'y' could refer to the
exposure, with all other variables listed in 'covs'.

## Usage

``` r
descMissData(y, covs, data, by = NULL, plot = FALSE)
```

## Arguments

- y:

  The analysis model outcome, specified as a string

- covs:

  The analysis model covariate(s), specified as a string (space
  delimited)

- data:

  A data frame containing the specified analysis model outcome,
  covariate(s), and if specified, stratification variable(s)

- by:

  Optional stratification variable(s), which must be a factor, or list
  of factors; if specified, the data are subsetted by the values of the
  factor(s) and missing data patterns are displayed for each subset in
  turn

- plot:

  If TRUE, displays a plot using
  [md.pattern](https://amices.org/mice/reference/md.pattern.html) to
  visualise the missing data patterns; if stratification variable(s) are
  specified, a separate plot will be displayed for each subset; use plot
  = FALSE (the default) to disable the plot

## Value

A summary of the missing data patterns

## Examples

``` r
descMissData(y="bmi7", covs="matage mated", data=bmi)
#> [[1]]
#>      pattern bmi7 matage mated   n pct
#> [1,]       1    1      1     1 592  59
#> [2,]       2    0      1     1 408  41
#> 
descMissData(y="bmi7", covs="matage mated bwt", by="pregsize", data=bmi)
#> pregsize: 0
#>      pattern bmi7 matage mated bwt   n pct
#> [1,]       1    1      1     1   1 524  59
#> [2,]       2    0      1     1   1 365  41
#> ------------------------------------------------------------ 
#> pregsize: 1
#>      pattern bmi7 matage mated bwt  n pct
#> [1,]       1    1      1     1   1 68  61
#> [2,]       2    0      1     1   1 43  39
```
