# Functionality for correspondence analysis ('correspondence') objects

These methods extract data from, and attribute new data to, objects of
class `"correspondence"` from the
**[MASS](https://rdrr.io/pkg/MASS/man/corresp.html)** package.

## Usage

``` r
# S3 method for class 'correspondence'
as_tbl_ord(x)

# S3 method for class 'correspondence'
recover_rows(x)

# S3 method for class 'correspondence'
recover_cols(x)

# S3 method for class 'correspondence'
recover_inertia(x)

# S3 method for class 'correspondence'
recover_conference(x)

# S3 method for class 'correspondence'
recover_coord(x)

# S3 method for class 'correspondence'
recover_aug_rows(x)

# S3 method for class 'correspondence'
recover_aug_cols(x)

# S3 method for class 'correspondence'
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
[`methods-cancor`](methods-cancor.md), [`methods-lda`](methods-lda.md),
[`methods-lra`](methods-lra.md), [`methods-mca`](methods-mca.md),
[`methods-prcomp`](methods-prcomp.md), [`methods-svd`](methods-svd.md)

Other models from the MASS package: [`methods-lda`](methods-lda.md),
[`methods-mca`](methods-mca.md)

## Examples

``` r
# table of hair and eye color data collapsed by sex
data(quine, package = "MASS")
class(quine)
#> [1] "data.frame"
head(quine)
#>   Eth Sex Age Lrn Days
#> 1   A   M  F0  SL    2
#> 2   A   M  F0  SL   11
#> 3   A   M  F0  SL   14
#> 4   A   M  F0  AL    5
#> 5   A   M  F0  AL    5
#> 6   A   M  F0  AL   13

# use correspondence analysis to construct row and column profiles
(quine_ca <- MASS::corresp(~ Age + Eth, data = quine))
#> First canonical correlation(s): 0.05317534 
#> 
#>  Age scores:
#>         F0         F1         F2         F3 
#> -0.3344445  1.4246090 -1.0320002 -0.4612728 
#> 
#>  Eth scores:
#>          A          N 
#> -1.0563816  0.9466276 
(quine_ca <- as_tbl_ord(quine_ca))
#> # A tbl_ord of class 'correspondence': (4 × 1) · (2 × 1)´
#> # 1 coordinate: Can1
#> # Rows (standard, 0%): [ 4 × 1 | 0 ]
#>     Can1 | 
#>      [1] | 
#> 1 -0.334 | 
#> 2  1.42  | 
#> 3 -1.03  | 
#> 4 -0.461 | 
#> # Columns (standard, 0%): [ 2 × 1 | 0 ]
#>     Can1 | 
#>      [1] | 
#> 1 -1.06  | 
#> 2  0.947 | 

# recover row and column profiles
get_rows(quine_ca)
#>          Can1
#> F0 -0.3344445
#> F1  1.4246090
#> F2 -1.0320002
#> F3 -0.4612728
get_cols(quine_ca)
#>         Can1
#> A -1.0563816
#> N  0.9466276

# augment profiles with names, masses, distances, and inertias
(quine_ca <- augment_ord(quine_ca))
#> # A tbl_ord of class 'correspondence': (4 × 1) · (2 × 1)´
#> # 1 coordinate: Can1
#> # Rows (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -0.334 | F0    active  
#> 2  1.42  | F1    active  
#> 3 -1.03  | F2    active  
#> 4 -0.461 | F3    active  
#> # Columns (standard, 0%): [ 2 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.06  | A     active  
#> 2  0.947 | N     active  
```
