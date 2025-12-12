# Inspect multiple imputation model

Check multiple imputation is valid under the proposed imputation model
and directed acyclic graph (DAG). Validity means that the proposed
approach will allow unbiased estimation of the estimand(s) of interest,
including regression parameters, associations, and causal effects. The
imputation model should include all other analysis model variables as
predictors, as well as any auxiliary variables. The DAG should include
all observed and unobserved variables related to the analysis model
variables and their missingness, as well as all required missingness
indicators.

## Usage

``` r
checkMI(dep, preds, r_dep, mdag)
```

## Arguments

- dep:

  The partially observed variable to be imputed, specified as a string

- preds:

  The imputation model predictor(s), specified as a string (space
  delimited)

- r_dep:

  The partially observed variable's missingness indicator, specified as
  a string

- mdag:

  The DAG, specified as a string using
  [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) syntax, or as
  a [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) graph object

## Value

A message indicating whether multiple imputation is valid under the
proposed DAG and imputation model

## Details

In principle, multiple imputation is valid if each partially observed
variable is unrelated to its own missingness, given its imputation model
predictors.

## References

Curnow E, Tilling K, Heron JE, Cornish RP, Carpenter JR. 2023. Multiple
imputation of missing data under missing at random: including a collider
as an auxiliary variable in the imputation model can induce bias.
Frontiers in Epidemiology. <doi:10.3389/fepid.2023.1237447>

## Examples

``` r
# Example DAG for which multiple imputation is valid, because collider
## variable 'bwt' is not included as a predictor
checkMI(dep="bmi7", preds="matage mated pregsize", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
              sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
              pregsize -> bwt sep_unmeas -> bwt")
#> Based on the proposed directed acyclic graph (DAG), the incomplete
#> variable and its missingness indicator are independent given imputation
#> model predictors. Hence, multiple imputation methods which assume data
#> are missing at random are valid in principle.

# Example DAG for which multiple imputation is not valid
checkMI(dep="bmi7", preds="matage", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r")
#> Based on the proposed directed acyclic graph (DAG), the incomplete
#> variable and its missingness indicator are not independent given
#> imputation model predictors. Hence, multiple imputation methods which
#> assume data are missing at random are not valid.
#> 
#> Consider using a different imputation model and/or strategy (e.g.
#> not-at-random fully conditional specification).  For example, the
#> incomplete variable and its missingness indicator are independent if
#> each of the following sets of variables are used as predictors in the
#> imputation model:
#> 
#> mated
#> 
#> c("matage", "mated")
#> 
#> sep_unmeas
#> 
#> c("matage", "sep_unmeas")
#> 
#> c("mated", "sep_unmeas")
#> 
#> c("matage", "mated", "sep_unmeas")
```
