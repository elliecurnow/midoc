# Performs reference-based multiple imputation

Creates multiple imputations using
[RefBasedMI](https://rdrr.io/pkg/RefBasedMI/man/RefBasedMI.html), based
on the dataset and relevant options specified by a call to
[proposeMI](https://elliecurnow.github.io/midoc/reference/proposeMI.md).
If a substantive model is specified, also calculates the pooled
estimates using [pool](https://amices.org/mice/reference/pool.html).

## Usage

``` r
doRefBasedMI(
  mipropobj,
  covs,
  depvar,
  treatvar,
  idvar,
  method,
  reference,
  seed,
  substmod = " ",
  message = TRUE
)
```

## Arguments

- mipropobj:

  An object of type 'miprop', created by a call to 'proposeMI'

- covs:

  The analysis model covariate(s), specified as a string (space
  delimited)

- depvar:

  The longitudinal outcome variable(s), specified as a string (space
  delimited)

- treatvar:

  Numeric treatment group variable; values must be positive integers

- idvar:

  Participant identifier variable

- method:

  Reference-based imputation method; methods that are supported are
  "J2R", "CR", and "CIR"

- reference:

  Numeric reference group for the specified method

- seed:

  An integer that is used to set the seed of the 'mice' call

- substmod:

  Optionally, a symbolic description of the substantive model to be
  fitted, specified as a string; if supplied, the model will be fitted
  to each imputed dataset and the results pooled

- message:

  If TRUE (the default), displays a message summarising the analysis
  that has been performed; use message = FALSE to suppress the message

## Value

A 'mice' object of class 'mids' (the multiply imputed datasets).
Optionally, a message summarising the analysis that has been performed.

## Details

The dataset is assumed to be in 'wide' format. Data are assumed to be
multivariate normal within each treatment arm. See
[RefBasedMI](https://rdrr.io/pkg/RefBasedMI/man/RefBasedMI.html) for
further details.

## Examples

``` r
if (FALSE) { # interactive()
# First specify the imputation model as a 'mimod' object
## (suppressing the message)
mimod_qol12 <- checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3",
                           family="gaussian(identity)",
                           data=qol,
                           message=FALSE)
# Save the proposed 'mice' options as a 'miprop' object
## (suppressing the message)
miprop_qol12 <- proposeMI(mimodobj=mimod_qol12,
                    data=qol,
                    message=FALSE,
                    plot = FALSE)
# Create the set of imputed datasets using the proposed 'mice' options and
## specified reference-based imputation method; then, fit the substantive
## model to each imputed dataset and display the pooled results
doRefBasedMI(mipropobj=miprop_qol12, covs="age0 qol0",
             depvar="qol3 qol12", treatvar="group",
             idvar="id", method="J2R", reference=1, seed=123,
             substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")
}
```
