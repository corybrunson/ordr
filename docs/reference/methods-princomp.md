# Functionality for principal components analysis ('princomp') objects

These methods extract data from, and attribute new data to, objects of
class `"princomp"` as returned by
[`stats::princomp()`](https://rdrr.io/r/stats/princomp.html).

## Usage

``` r
# S3 method for class 'princomp'
as_tbl_ord(x)

# S3 method for class 'princomp'
recover_rows(x)

# S3 method for class 'princomp'
recover_cols(x)

# S3 method for class 'princomp'
recover_inertia(x)

# S3 method for class 'princomp'
recover_coord(x)

# S3 method for class 'princomp'
recover_conference(x)

# S3 method for class 'princomp'
recover_supp_rows(x)

# S3 method for class 'princomp'
recover_aug_rows(x)

# S3 method for class 'princomp'
recover_aug_cols(x)

# S3 method for class 'princomp'
recover_aug_coord(x)
```

## Arguments

- x:

  An ordination object.

## Value

The recovery generics `recover_*()` return [core model
components](recoverers.md), [distribution of inertia](conference.md),
[supplementary elements](supplementation.md), and [intrinsic
metadata](augmentation.md); but they require methods for each model
class to tell them what these components are.

The generic [`as_tbl_ord()`](tbl_ord.md) returns its input wrapped in
the 'tbl_ord' class. Its methods determine what model classes it is
allowed to wrap. It then provides 'tbl_ord' methods with access to the
recoverers and hence to the model components.

## Details

Principal components analysis (PCA), as performed by
[`stats::princomp()`](https://rdrr.io/r/stats/princomp.html), relies on
an eigenvalue decomposition (EVD) of the covariance matrix \\X^TX\\ of a
data set \\X\\.
[`stats::princomp()`](https://rdrr.io/r/stats/princomp.html) returns the
EVD factor \\V\\ as the loadings `$loadings`. The scores `$scores` are
obtained as \\XV\\ and are accessible as supplementary elements.

## See also

Other methods for eigen-decomposition-based techniques:
[`methods-cmds`](methods-cmds.md), [`methods-eigen`](methods-eigen.md),
[`methods-factanal`](methods-factanal.md)

Other models from the stats package:
[`methods-cancor`](methods-cancor.md),
[`methods-cmds`](methods-cmds.md),
[`methods-factanal`](methods-factanal.md),
[`methods-kmeans`](methods-kmeans.md), [`methods-lm`](methods-lm.md),
[`methods-prcomp`](methods-prcomp.md)

## Author

Emily Paul, John Gracey

## Examples

``` r
# data frame of Anderson iris species measurements
class(iris)
#> [1] "data.frame"
head(iris)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

# compute unscaled row-principal components of scaled measurements
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  print() -> iris_pca
#> # A tbl_ord of class 'princomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: Comp.1, Comp.2, ..., Comp.4
#> # Rows (principal, 100%): [ 150 × 4 | 0 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | 
#>     [630] [36.16] [11.65]  [3.551] | 
#> 1 -2.68    0.319   0.0279  0.00226 | 
#> 2 -2.71   -0.177   0.210   0.0990  | 
#> 3 -2.89   -0.145  -0.0179  0.0200  | 
#> 4 -2.75   -0.318  -0.0316 -0.0756  | 
#> 5 -2.73    0.327  -0.0901 -0.0613  | 
#>                 ⋮                    
#> # Columns (standard, 0%): [ 4 × 4 | 0 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | 
#>       [1]     [1]     [1]      [1] | 
#> 1  0.361   0.657   0.582   0.315   | 
#> 2 -0.0845  0.730  -0.598  -0.320   | 
#> 3  0.857  -0.173  -0.0762 -0.480   | 
#> 4  0.358  -0.0755 -0.546   0.754   | 

# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
#>         Comp.1     Comp.2      Comp.3       Comp.4
#> [1,] -2.684126  0.3193972  0.02791483  0.002262437
#> [2,] -2.714142 -0.1770012  0.21046427  0.099026550
#> [3,] -2.888991 -0.1449494 -0.01790026  0.019968390
#> [4,] -2.745343 -0.3182990 -0.03155937 -0.075575817
#> [5,] -2.728717  0.3267545 -0.09007924 -0.061258593
#> [6,] -2.280860  0.7413304 -0.16867766 -0.024200858
get_cols(iris_pca)
#>                   Comp.1      Comp.2      Comp.3     Comp.4
#> Sepal.Length  0.36138659  0.65658877  0.58202985  0.3154872
#> Sepal.Width  -0.08452251  0.73016143 -0.59791083 -0.3197231
#> Petal.Length  0.85667061 -0.17337266 -0.07623608 -0.4798390
#> Petal.Width   0.35828920 -0.07548102 -0.54583143  0.7536574

# augment measurement coordinates with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
#> # A tbl_ord of class 'princomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: Comp.1, Comp.2, ..., Comp.4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | .element
#>     [630] [36.16] [11.65]  [3.551] | <chr>   
#> 1 -2.68    0.319   0.0279  0.00226 | score   
#> 2 -2.71   -0.177   0.210   0.0990  | score   
#> 3 -2.89   -0.145  -0.0179  0.0200  | score   
#> 4 -2.75   -0.318  -0.0316 -0.0756  | score   
#> 5 -2.73    0.327  -0.0901 -0.0613  | score   
#>                 ⋮                       ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 4 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | name       center scale
#>       [1]     [1]     [1]      [1] | <chr>       <dbl> <dbl>
#> 1  0.361   0.657   0.582   0.315   | Sepal.Len…   5.84     1
#> 2 -0.0845  0.730  -0.598  -0.320   | Sepal.Wid…   3.06     1
#> 3  0.857  -0.173  -0.0762 -0.480   | Petal.Len…   3.76     1
#> 4  0.358  -0.0755 -0.546   0.754   | Petal.Wid…   1.20     1
#> # ℹ 1 more variable:
#> #   .element <chr>
```
