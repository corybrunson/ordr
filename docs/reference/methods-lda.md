# Functionality for linear discriminant analysis ('lda') objects

These methods extract data from, and attribute new data to, objects of
class `"lda"` and `"lda_ord"` as returned by
[`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) and
[`lda_ord()`](lda-ord.md).

## Usage

``` r
# S3 method for class 'lda'
as_tbl_ord(x)

# S3 method for class 'lda_ord'
as_tbl_ord(x)

# S3 method for class 'lda'
recover_rows(x)

# S3 method for class 'lda_ord'
recover_rows(x)

# S3 method for class 'lda'
recover_cols(x)

# S3 method for class 'lda_ord'
recover_cols(x)

# S3 method for class 'lda'
recover_inertia(x)

# S3 method for class 'lda_ord'
recover_inertia(x)

# S3 method for class 'lda'
recover_coord(x)

# S3 method for class 'lda_ord'
recover_coord(x)

# S3 method for class 'lda'
recover_conference(x)

# S3 method for class 'lda_ord'
recover_conference(x)

# S3 method for class 'lda'
recover_aug_rows(x)

# S3 method for class 'lda_ord'
recover_aug_rows(x)

# S3 method for class 'lda'
recover_aug_cols(x)

# S3 method for class 'lda_ord'
recover_aug_cols(x)

# S3 method for class 'lda'
recover_aug_coord(x)

# S3 method for class 'lda_ord'
recover_aug_coord(x)

# S3 method for class 'lda'
recover_supp_rows(x)

# S3 method for class 'lda_ord'
recover_supp_rows(x)
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

See [lda-ord](lda-ord.md) for details.

## See also

Other methods for singular value decomposition-based techniques:
[`methods-cancor`](methods-cancor.md),
[`methods-correspondence`](methods-correspondence.md),
[`methods-lra`](methods-lra.md), [`methods-mca`](methods-mca.md),
[`methods-prcomp`](methods-prcomp.md), [`methods-svd`](methods-svd.md)

Other models from the MASS package:
[`methods-correspondence`](methods-correspondence.md),
[`methods-mca`](methods-mca.md)

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

# default (unstandardized discriminant) coefficients
lda_ord(iris[, 1:4], iris[, 5]) %>%
  as_tbl_ord() %>%
  print() -> iris_lda
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 0 ]
#>      LD1     LD2 | 
#>   [2366] [20.98] | 
#> 1  7.61  -0.215  | 
#> 2 -1.83   0.728  | 
#> 3 -5.78  -0.513  | 
#> 4  8.06  -0.300  | 
#> 5  7.13   0.787  | 
#>        ⋮           
#> # Columns (standard, 0%): [ 4 × 2 | 0 ]
#>      LD1     LD2 | 
#>      [1]     [1] | 
#> 1  0.829 -0.0241 | 
#> 2  1.53  -2.16   | 
#> 3 -2.20   0.932  | 
#> 4 -2.81  -2.84   | 

# recover centroid coordinates and measurement discriminant coefficients
get_rows(iris_lda, elements = "active")
#>                  LD1        LD2
#> setosa      7.607600 -0.2151330
#> versicolor -1.825049  0.7278996
#> virginica  -5.782550 -0.5127666
head(get_rows(iris_lda, elements = "score"))
#>           LD1        LD2
#> [1,] 8.061800 -0.3004206
#> [2,] 7.128688  0.7866604
#> [3,] 7.489828  0.2653845
#> [4,] 6.813201  0.6706311
#> [5,] 8.132309 -0.5144625
#> [6,] 7.701947 -1.4617210
get_cols(iris_lda)
#>                     LD1         LD2
#> Sepal.Length  0.8293776 -0.02410215
#> Sepal.Width   1.5344731 -2.16452123
#> Petal.Length -2.2012117  0.93192121
#> Petal.Width  -2.8104603 -2.83918785

# augment ordination with centroid and measurement names
augment_ord(iris_lda)
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 5 ]
#>      LD1     LD2 | name        prior counts grouping   .element
#>   [2366] [20.98] | <chr>       <dbl>  <int> <chr>      <chr>   
#> 1  7.61  -0.215  | setosa      0.333     50 setosa     active  
#> 2 -1.83   0.728  | versicolor  0.333     50 versicolor active  
#> 3 -5.78  -0.513  | virginica   0.333     50 virginica  active  
#> 4  8.06  -0.300  | NA         NA         NA setosa     score   
#> 5  7.13   0.787  | NA         NA         NA setosa     score   
#>        ⋮                                ⋮                      
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name         .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  
```
