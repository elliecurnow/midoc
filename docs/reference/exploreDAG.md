# Compares data with proposed DAG

Explore the relationships implied by the proposed directed acyclic graph
(DAG). Optionally, if a dataset is supplied, explore whether
relationships between fully observed variables in the specified dataset
are consistent with the proposed DAG.

## Usage

``` r
exploreDAG(mdag, data = NULL)
```

## Arguments

- mdag:

  The DAG, specified as a string using
  [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) syntax, or as
  a [dagitty](https://rdrr.io/pkg/dagitty/man/dagitty.html) graph object

- data:

  Optionally, a data frame containing all the variables stated in the
  DAG. All ordinal variables must be integer-coded and all categorical
  variables must be dummy-coded.

## Value

A message listing the pairs of variables that are implied to be
independent (possibly conditional on other variables) by the proposed
DAG. Optionally, if a dataset is supplied, a message indicating whether
the relationships between fully observed variables in the specified
dataset are consistent with the proposed DAG.

## Examples

``` r
exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7
                 sep_unmeas -> mated sep_unmeas -> r",
           data=bmi)
#> The proposed directed acyclic graph (DAG) implies the following pairs
#> of variables are (conditionally) independent (where, for example, 'X
#> _||_ Y | Z' should be read as 'X is independent of Y conditional on
#> Z'). Note that variable names are abbreviated. Consider whether these
#> (conditional) independencies are plausible for your study, and update
#> your DAG accordingly:
#> 
#> bmi7 _||_ r | sp_n
#> 
#> bmi7 _||_ r | matd
#> 
#> bmi7 _||_ sp_n | matd
#> 
#> matg _||_ r | sp_n
#> 
#> matg _||_ r | matd
#> 
#> matg _||_ sp_n | matd
#> 
#> matd _||_ r | sp_n
#> 
#> These (conditional) independence statements are explored below using
#> the canonical correlations approach for mixed data. See
#> ??dagitty::localTests for further details.  Results are shown for
#> variables that are fully observed in the specified dataset. The null
#> hypothesis is that the stated variables are (conditionally)
#> independent.
#> 
#>                         estimate  p.value        2.5%      97.5%
#> 
#> matage _||_ r | mated 0.02998323 0.343547 -0.03206946 0.09180567
#> 
#> Interpretation: A small p-value means the stated variables may not be
#> (conditionally) independent in the specified dataset: your data may not
#> be consistent with the proposed DAG. A large p-value means there is
#> little evidence of inconsistency between your data and the proposed
#> DAG.
#> 
#> Note that there may also be other DAGs which your data are consistent
#> with. Also note that these results assume that relationships between
#> variables are linear. Consider exploring the specification of each
#> relationship in your model.  Also consider whether it is valid and
#> possible to explore relationships between partially observed variables
#> using the observed data, e.g. avoiding perfect prediction.
```
