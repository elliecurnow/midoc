# Performs reference-based multiple imputation

Creates multiple imputations under reference-based multiple imputation
using [RefBasedMI](https://rdrr.io/pkg/RefBasedMI/man/RefBasedMI.html).
Imputations are based on the dataset and relevant options specified by a
call to
[proposeMI](https://elliecurnow.github.io/midoc/reference/proposeMI.md).
If a substantive model is specified, the pooled estimates are also
calculated using [pool](https://amices.org/mice/reference/pool.html).
The dataset is assumed to be in 'wide' format, with one row per subject.
It is assumed that the outcome is measured repeatedly over time, with at
least one measurement at an intermediate time-point (i.e. between
baseline and study end-point). Data are assumed to be multivariate
normal within each category of the grouping variable (in typical use,
this denotes the treatment allocation).

## Usage

``` r
doRefBasedMI(
  mipropobj,
  y,
  groupvar,
  covs = NULL,
  idvar = NULL,
  method,
  reference,
  seed,
  substmod = NULL,
  message = TRUE
)
```

## Arguments

- mipropobj:

  An object of type 'miprop', created by a call to 'proposeMI'

- y:

  The analysis model outcome variables (at least two are required),
  specified as a string (space delimited) or a list

- groupvar:

  Group variable; can be numeric or string

- covs:

  Optional analysis model covariate(s), specified as a string (space
  delimited) or a list; a maximum of five covariates can be specified

- idvar:

  Optional participant identifier variable; if not provided, an
  identifier variable, named 'id', will be automatically created

- method:

  Reference-based imputation method; methods that are supported are
  "J2R", "CR", and "CIR"

- reference:

  Reference group for the specified method; can be numeric or string

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

Reference-based multiple imputation uses observed data from one category
of the grouping variable - the 'reference' group - to impute missing
values in other categories. Available reference-based methods are
'jump-to-reference' (J2R), 'copy reference' (CR), and 'copy increments
in reference' (CIR). J2R assumes that the distribution of outcomes for
individuals who drop out 'jumps to' the distribution observed in the
reference group following their last observed time point. CR assumes
individuals who drop out behave as if they are in the specified
reference group for the full duration of the trial. CIR assumes that the
distribution of outcomes for individuals who drop out follows the mean
increments observed in the reference group, following their last
observed time point.

## Examples

``` r
if (FALSE) { # interactive()
# First specify the imputation model as a 'mimod'object
## (suppressing the message)
mimod_qol12 <- checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 +
  qol3", family="gaussian(identity)", data=qol, message=FALSE)

# Save the proposed 'mice' options as a 'miprop' object
## (suppressing the message)
miprop_qol12 <- proposeMI(mimodobj=mimod_qol12, data=qol, message=FALSE,
  plot = FALSE)

# Create the set of imputed datasets using the proposed mice' options and
## specified reference-based imputation method; then fit the substantive
## model to each imputed dataset and display the pooled results
doRefBasedMI(mipropobj=miprop_qol12, y="qol3 qol12", groupvar="group",
 covs="age0 qol0", idvar="id", method="J2R", reference=1, seed=123,
 substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")
}
```
