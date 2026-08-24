# Functionality for principal components analysis ('prcomp') objects

These methods extract data from, and attribute new data to, objects of
class `"prcomp"` as returned by
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html).

## Usage

``` r
# S3 method for class 'prcomp'
as_tbl_ord(x)

# S3 method for class 'prcomp'
recover_rows(x)

# S3 method for class 'prcomp'
recover_cols(x)

# S3 method for class 'prcomp'
recover_inertia(x)

# S3 method for class 'prcomp'
recover_coord(x)

# S3 method for class 'prcomp'
recover_conference(x)

# S3 method for class 'prcomp'
recover_aug_rows(x)

# S3 method for class 'prcomp'
recover_aug_cols(x)

# S3 method for class 'prcomp'
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

## See also

Other methods for singular value decomposition-based techniques:
[`methods-cancor`](methods-cancor.md),
[`methods-correspondence`](methods-correspondence.md),
[`methods-lda`](methods-lda.md), [`methods-lra`](methods-lra.md),
[`methods-mca`](methods-mca.md), [`methods-svd`](methods-svd.md)

Other models from the stats package:
[`methods-cancor`](methods-cancor.md),
[`methods-cmds`](methods-cmds.md),
[`methods-factanal`](methods-factanal.md),
[`methods-kmeans`](methods-kmeans.md), [`methods-lm`](methods-lm.md),
[`methods-princomp`](methods-princomp.md)

## Author

Emily Paul

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

# compute scaled row-principal components of scaled measurements
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 0 ]
#>      PC1     PC2     PC3     PC4 | 
#>  [434.9] [136.2] [21.87] [3.087] | 
#> 1 -2.26  -0.478   0.127   0.0241 | 
#> 2 -2.07   0.672   0.234   0.103  | 
#> 3 -2.36   0.341  -0.0441  0.0283 | 
#> 4 -2.29   0.595  -0.0910 -0.0657 | 
#> 5 -2.38  -0.645  -0.0157 -0.0358 | 
#>                ⋮                   
#> # Columns (standard, 0%): [ 4 × 4 | 0 ]
#>      PC1     PC2     PC3     PC4 | 
#>      [1]     [1]     [1]     [1] | 
#> 1  0.521 -0.377   0.720   0.261  | 
#> 2 -0.269 -0.923  -0.244  -0.124  | 
#> 3  0.580 -0.0245 -0.142  -0.801  | 
#> 4  0.565 -0.0669 -0.634   0.524  | 

# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
#>            PC1        PC2         PC3          PC4
#> [1,] -2.257141 -0.4784238  0.12727962  0.024087508
#> [2,] -2.074013  0.6718827  0.23382552  0.102662845
#> [3,] -2.356335  0.3407664 -0.04405390  0.028282305
#> [4,] -2.291707  0.5953999 -0.09098530 -0.065735340
#> [5,] -2.381863 -0.6446757 -0.01568565 -0.035802870
#> [6,] -2.068701 -1.4842053 -0.02687825  0.006586116
get_cols(iris_pca)
#>                     PC1         PC2        PC3        PC4
#> Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
#> Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
#> Petal.Length  0.5804131 -0.02449161 -0.1421264 -0.8014492
#> Petal.Width   0.5648565 -0.06694199 -0.6342727  0.5235971

# augment measurements with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>      PC1     PC2     PC3     PC4 | .element
#>  [434.9] [136.2] [21.87] [3.087] | <chr>   
#> 1 -2.26  -0.478   0.127   0.0241 | active  
#> 2 -2.07   0.672   0.234   0.103  | active  
#> 3 -2.36   0.341  -0.0441  0.0283 | active  
#> 4 -2.29   0.595  -0.0910 -0.0657 | active  
#> 5 -2.38  -0.645  -0.0157 -0.0358 | active  
#>                ⋮                      ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 4 ]
#>      PC1     PC2     PC3     PC4 | name       center scale
#>      [1]     [1]     [1]     [1] | <chr>       <dbl> <dbl>
#> 1  0.521 -0.377   0.720   0.261  | Sepal.Len…   5.84 0.828
#> 2 -0.269 -0.923  -0.244  -0.124  | Sepal.Wid…   3.06 0.436
#> 3  0.580 -0.0245 -0.142  -0.801  | Petal.Len…   3.76 1.77 
#> 4  0.565 -0.0669 -0.634   0.524  | Petal.Wid…   1.20 0.762
#> # ℹ 1 more variable:
#> #   .element <chr>
```
