# Inspect multiple imputation model

Check multiple imputation is valid under the proposed imputation
model(s) and directed acyclic graph (DAG). Validity means that the
proposed approach will allow unbiased estimation of the estimand(s) of
interest, including regression parameters, associations, and causal
effects.

## Usage

``` r
checkMI(dep, preds = NULL, r_cra, mdag)
```

## Arguments

- dep:

  The partially observed variable(s) to be imputed, specified as a
  string (space delimited) or a list

- preds:

  Optional fully observed imputation model predictor(s), specified as a
  string (space delimited) or a list

- r_cra:

  The complete record indicator, specified as a string

- mdag:

  The DAG, specified as a string using
  [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) syntax, or as
  a [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) graph object

## Value

A message indicating whether multiple imputation is valid under the
proposed DAG and imputation model predictor(s)

## Details

Imputation model(s) should include all other analysis model variables as
predictors, as well as any auxiliary variables. The DAG should include
all observed and unobserved variables related to the analysis model
variables and their missingness, as well as the complete record
("missingness") indicator.

In principle, multiple imputation is valid if all partially observed
variables are unrelated to missingness, given the (fully observed)
imputation model predictors. This is determined using the proposed DAG
by checking whether all the partially observed variables are
'd-separated' from the complete record indicator, conditional on the
imputation model predictors. It is assumed that all the specified
imputation model predictors are fully observed and will be used to
impute all the specified partially observed variables.

## References

Curnow E, Tilling K, Heron JE, Cornish RP, Carpenter JR. 2023. Multiple
imputation of missing data under missing at random: including a collider
as an auxiliary variable in the imputation model can induce bias.
Frontiers in Epidemiology. <doi:10.3389/fepid.2023.1237447>

## Examples

``` r
# Example DAG for which multiple imputation is valid, because collider
## variable 'bwt' is not included as a predictor
checkMI(dep="bmi7", preds="matage mated pregsize", r_cra="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
              sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
              pregsize -> bwt sep_unmeas -> bwt")
#> Based on the proposed directed acyclic graph (DAG), the partially
#> observed variable(s) and complete record indicator are independent
#> given the fully observed imputation model predictor(s). Hence, multiple
#> imputation methods which assume data are missing at random are valid in
#> principle.

# Example DAG for which multiple imputation is not valid
checkMI(dep="bmi7", preds="matage", r_cra="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r")
#> Based on the proposed directed acyclic graph (DAG), the partially
#> observed variable(s) and complete record indicator are not independent
#> given the fully observed imputation model predictor(s). Hence, multiple
#> imputation methods which assume data are missing at random may not be
#> not valid.
#> 
#> Consider using a different imputation model and/or strategy (e.g.
#> not-at-random fully conditional specification).  For example, the
#> partially observed variable(s) and complete record indicator are
#> independent if each of the following sets of variables are used as
#> predictors in the imputation model(s):
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
