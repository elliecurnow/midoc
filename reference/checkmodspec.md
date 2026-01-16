# Specify and inspect parametric model specification

Specify a parametric model (which may represent the analysis or
imputation model). Optionally, if a dataset is supplied, explore whether
the observed relationships in the specified dataset are consistent with
the proposed parametric model.

## Usage

``` r
checkModSpec(formula, family, data = NULL, plot = TRUE, message = TRUE)
```

## Arguments

- formula:

  A symbolic description of the model to be fitted, with the dependent
  variable on the left of a ~ operator, and the covariates, separated
  by + operators, on the right, specified as a string

- family:

  A description of the error distribution and link function to be used
  in the model, specified as a string; family functions that are
  supported are "gaussian(identity)" and "binomial(logit)"

- data:

  Optionally, a data frame containing all the variables stated in the
  formula

- plot:

  If TRUE (the default), and a dataset is supplied, displays a plot
  which can be used to explore the functional form of each covariate in
  the specified model; use plot = FALSE to disable the plot

- message:

  If TRUE (the default), and a dataset is supplied, displays a message
  indicating whether the relationships between the dependent variable
  and covariates are likely to be correctly specified or not; use
  message = FALSE to suppress the message

## Value

An object of type 'mimod' (a list containing the specified formula,
family, and, if specified, dataset name). Optionally, if required and a
dataset is supplied, a message indicating whether the relationships
between the dependent variable and covariates are likely to be correctly
specified or not. If there is evidence of model mis-specification,
optionally returns a plot of the model residuals versus the fitted
values which can be used to explore the appropriate functional form for
the specified model.

## References

Curnow E, Carpenter JR, Heron JE, et al. 2023. Multiple imputation of
missing data under missing at random: compatible imputation models are
not sufficient to avoid bias if they are mis-specified. J Clin
Epidemiol. <doi:10.1016/j.jclinepi.2023.06.011>

## Examples

``` r
# Example (incorrectly) assuming a linear relationship
checkModSpec(formula="bmi7~matage+mated+pregsize",
             family="gaussian(identity)", data=bmi)
#> Method used to explore the relationship between the model residuals (y)
#> and fitted values (fitvals): regression of model residuals on a
#> fractional polynomial of the fitted values
#> 
#> Call:
#> 
#> glm(formula = y ~ ., family = family, data = data, weights = weights, 
#> 
#>     offset = offset, x = TRUE, y = TRUE)
#> 
#> Coefficients:
#> 
#>               Estimate Std. Error t value Pr(>|t|)    
#> 
#> (Intercept) -5.908e-15  5.512e-02    0.00        1    
#> 
#> fitvals.1   -5.268e-02  3.215e-03  -16.39   <2e-16 ***
#> 
#> fitvals.2    1.629e-02  9.926e-04   16.41   <2e-16 ***
#> 
#> ---
#> 
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for gaussian family taken to be 1.798351)
#> 
#>     Null deviance: 1547.8  on 591  degrees of freedom
#> 
#> Residual deviance: 1059.2  on 589  degrees of freedom
#> 
#> AIC: 2032.4
#> 
#> Number of Fisher Scoring iterations: 2
#> 
#> Interpretation: A weak relationship between the model residuals and
#> fitted values means there is little evidence of model
#> mis-specification. A strong relationship between the model residuals
#> and fitted values means the model may be mis-specified.
#> 
#> Consider whether the specified model is plausible for your study, and
#> update it accordingly.  Note that the observed relationships may be
#> distorted by data missing not at random.

  ## For the example above, (correctly) assuming a quadratic relationship
checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
             family="gaussian(identity)", data=bmi)
#> Method used to explore the relationship between the model residuals (y)
#> and fitted values (fitvals): regression of model residuals on a
#> fractional polynomial of the fitted values
#> 
#> Call:
#> 
#> glm(formula = y ~ ., family = family, data = data, weights = weights, 
#> 
#>     offset = offset, x = TRUE, y = TRUE)
#> 
#> Coefficients:
#> 
#>               Estimate Std. Error t value Pr(>|t|)
#> 
#> (Intercept) -3.409e-17  4.560e-02       0        1
#> 
#> (Dispersion parameter for gaussian family taken to be 1.230712)
#> 
#>     Null deviance: 727.35  on 591  degrees of freedom
#> 
#> Residual deviance: 727.35  on 591  degrees of freedom
#> 
#> AIC: 1805.9
#> 
#> Number of Fisher Scoring iterations: 2
#> 
#> Interpretation: A weak relationship between the model residuals and
#> fitted values means there is little evidence of model
#> mis-specification. A strong relationship between the model residuals
#> and fitted values means the model may be mis-specified.
#> 
#> Consider whether the specified model is plausible for your study, and
#> update it accordingly.  Note that the observed relationships may be
#> distorted by data missing not at random.
```
