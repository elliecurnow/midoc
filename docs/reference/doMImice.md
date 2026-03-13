# Performs multiple imputation

Creates multiple imputations using
[mice](https://amices.org/mice/reference/mice.html), based on the
options and dataset specified by a call to
[proposeMI](https://elliecurnow.github.io/midoc/reference/proposeMI.md).
If a substantive model is specified, also calculates the pooled
estimates using [pool](https://amices.org/mice/reference/pool.html).

## Usage

``` r
doMImice(mipropobj, seed, substmod = " ", message = TRUE)
```

## Arguments

- mipropobj:

  An object of type 'miprop', created by a call to 'proposeMI'

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

## Examples

``` r
# First specify the imputation model as a 'mimod' object
## (suppressing the message)
mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
                           family="gaussian(identity)",
                           data=bmi,
                           message=FALSE)

# Save the proposed 'mice' options as a 'miprop' object
## (suppressing the message)
miprop <- proposeMI(mimodobj=mimod_bmi7,
                    data=bmi,
                    message=FALSE,
                    plot = FALSE)
# Create the set of imputed datasets using the proposed 'mice' options
imp <- doMImice(miprop,123)
#> Now you have created your multiply imputed datasets, you can perform
#> your analysis and pool the results using the 'mice' functions 'with()'
#> and 'pool()'

# Additionally, fit the substantive model to each imputed dataset and display
## the pooled results
doMImice(miprop, 123, substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")
#> Given the substantive model: lm(bmi7 ~ matage + I(matage^2) + mated) ,
#> multiple imputation estimates are as follows:
#> 
#>          term   estimate  std.error  statistic       df       p.value
#> 
#> 1 (Intercept) 17.6607324 0.07126548 247.816079 233.1668 2.116834e-284
#> 
#> 2      matage  1.1504545 0.05230345  21.995769 184.5081  1.863532e-53
#> 
#> 3 I(matage^2)  0.8414975 0.03231752  26.038433 257.1270  4.754845e-74
#> 
#> 4      mated1 -1.0026194 0.10787751  -9.294054 159.1101  1.094881e-16
#> 
#>        2.5 %     97.5 %   conf.low  conf.high
#> 
#> 1 17.5203258 17.8011389 17.5203258 17.8011389
#> 
#> 2  1.0472648  1.2536442  1.0472648  1.2536442
#> 
#> 3  0.7778567  0.9051382  0.7778567  0.9051382
#> 
#> 4 -1.2156760 -0.7895629 -1.2156760 -0.7895629
```
