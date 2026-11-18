# Measures of goodness of fit of ordination models

Compute the quality, adequacy, and predictivity of a 'tbl_ord' object
from the retrieved matrix factors.

## Usage

``` r
ord_quality(x, rank = NULL)

ord_adequacy(x, .matrix, rank = NULL)

ord_predictivity(x, .matrix, rank = NULL)
```

## Arguments

- x:

  A 'tbl_ord' object.

- rank:

  The maximum rank for which to compute statistics; the default, `NULL`
  computes statistics up to the rank of the model.

- .matrix:

  A character string partially matched (lowercase) to several indicators
  for one or both matrices in a matrix decomposition used for
  ordination. The standard values are `"rows"`, `"cols"`, and `"dims"`
  (for both).

## Value

A vector, matrix, or list of matrices of numeric goodness-of-fit
statistics. If no items are found, a matrix will have zero rows.

## Details

Gower, Gardner–Lubbe, & le Roux (2011) detail several measures of fit
for biplots, most prominently

- the *quality* of the \\r\\-dimensional biplot, measured as the
  proportion of variance in the plot, calculated as the quotient of the
  traces of \\\Lambda_r = {D_r}^2\\ and of \\\Lambda = D^2\\.

- the *adequacy* of the representation of the \\j\\-th row
  (respectively, column) in the \\r\\-dimensional biplot, calculated as
  the \\j\\-th diagonal element of \\U_r\\ {U_r}^\top\\ (respectively,
  \\V_r\\ {V_r}^\top\\), understood as the fidelity of the projections
  of the standard coordinates.

- the *predictivity* of the \\j\\-th row (respectively, column) in the
  \\r\\-dimensional biplot, measured as the quotient of the \\j\\-th
  diagonal elements of \\U_r\\ \Lambda_r\\ {U_r}^\top\\ and of \\U\\
  \Lambda\\ U^\top\\ (respectively, of \\V_r\\ \Lambda_r\\ {V_r}^\top\\
  and of \\V\\ \Lambda\\ V^\top\\), understood as the fidelity of the
  projections of the principal coordinates.

These can be calculated directly from any SVD or EVD and interpreted for
any technique based on them. In some cases they may also be calculated
for supplementary elements.

## References

Gower JC, Gardner–Lubbe S, & le Roux NJ (2011) *Understanding Biplots*.
Wiley, ISBN: 978-0-470-01255-0. <https://www.wiley.com/go/biplots>

## Examples

``` r
# log-ratio analysis of Apollonia glass composition data
glass_apollonia <- subset(
  glass,
  Site == "Apollonia",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
glass_lra <- lra(glass_apollonia, weighted = FALSE)

# quality (cumulative proportion of inertia included)
ord_quality(glass_lra)
#>     LRSV1     LRSV2     LRSV3     LRSV4     LRSV5 
#> 0.7969324 0.9409377 0.9784321 0.9959841 1.0000000 
# adequacy (fidelity of projections to standard coordinates)
ord_adequacy(glass_lra, "rows", rank = 3)
#>            LRSV1     LRSV2     LRSV3
#>  [1,] 2.44976551 4.7558152 4.7635541
#>  [2,] 0.91800463 0.9592602 1.0818327
#>  [3,] 0.05196923 0.1195347 4.6706254
#>  [4,] 3.93403376 4.3653784 4.4117974
#>  [5,] 0.71014365 0.7305881 0.8202009
#>  [6,] 0.24370668 1.8973926 1.9831029
#>  [7,] 0.13882111 1.8900441 1.9995351
#>  [8,] 0.37241161 0.5436302 4.2700337
#>  [9,] 0.18114381 2.7383564 2.9993178
ord_adequacy(glass_lra, "cols", rank = 3)
#>             LRSV1     LRSV2     LRSV3
#> SiO2  0.415710347 1.3192747 1.3249105
#> Na2O  0.332761756 0.9780515 1.7071512
#> CaO   0.001858223 2.7192156 4.7648583
#> Al2O3 0.498393983 0.7128763 0.9445059
#> MgO   0.077021823 1.3051068 4.2686261
#> K2O   4.674253868 4.9654752 4.9899481
# predictivity (fidelity of projections to principal coordinates)
ord_predictivity(glass_lra, "dims", rank = 2)
#> [[1]]
#>           LRSV1     LRSV2
#>  [1,] 0.8536441 0.9988480
#>  [2,] 0.9797510 0.9877073
#>  [3,] 0.1732252 0.2139208
#>  [4,] 0.9771112 0.9964704
#>  [5,] 0.9459268 0.9508477
#>  [6,] 0.3645607 0.8115659
#>  [7,] 0.3001007 0.9841858
#>  [8,] 0.6320160 0.6845225
#>  [9,] 0.2627951 0.9331702
#> 
#> [[2]]
#>             LRSV1     LRSV2
#> SiO2  0.681579145 0.9492751
#> Na2O  0.601363376 0.8120881
#> CaO   0.003126896 0.8293923
#> Al2O3 0.843053946 0.9086128
#> MgO   0.170390115 0.6613164
#> K2O   0.988606882 0.9997368
#> 

# principal components analysis of setosa iris data
iris_pca <- princomp(iris3[, , "Setosa"])

# quality (cumulative proportion of inertia included)
ord_quality(iris_pca)
#>    Comp.1    Comp.2    Comp.3    Comp.4 
#> 0.7647237 0.8841229 0.9707854 1.0000000 
# adequacy (fidelity of projections to standard coordinates)
ord_adequacy(iris_pca, "both")
#> [[1]]
#>      Comp.1 Comp.2 Comp.3 Comp.4
#> 
#> [[2]]
#>               Comp.1     Comp.2    Comp.3 Comp.4
#> Sepal L. 0.447665911 0.80513120 0.9986984      1
#> Sepal W. 0.538973034 0.92420852 0.9996178      1
#> Petal L. 0.009320724 0.24947521 0.9424474      1
#> Petal W. 0.004040331 0.02118507 0.0592364      1
#> 
# predictivity (fidelity of projections to principal coordinates)
ord_predictivity(iris_pca, "f")
#>      Comp.1 Comp.2 Comp.3 Comp.4
ord_predictivity(iris_pca, "g")
#>              Comp.1    Comp.2    Comp.3 Comp.4
#> Sepal L. 0.85194383 0.9581593 0.9999054      1
#> Sepal W. 0.88693313 0.9859131 0.9999760      1
#> Petal L. 0.07307686 0.3670569 0.9827619      1
#> Petal W. 0.08602094 0.1430131 0.2348218      1
```
