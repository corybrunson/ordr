# Functionality for eigen-decompositions

These methods extract data from, and attribute new data to, objects of
class `"eigen"` returned by
[`base::eigen()`](https://rdrr.io/r/base/eigen.html) when the parameter
`only.values` is set to `FALSE` or of class `"eigen_ord"` returned by
[`eigen_ord()`](wrap-ord.md).

## Usage

``` r
# S3 method for class 'eigen'
as_tbl_ord(x)

# S3 method for class 'eigen'
recover_rows(x)

# S3 method for class 'eigen'
recover_cols(x)

# S3 method for class 'eigen'
recover_inertia(x)

# S3 method for class 'eigen'
recover_coord(x)

# S3 method for class 'eigen'
recover_conference(x)

# S3 method for class 'eigen'
recover_aug_rows(x)

# S3 method for class 'eigen'
recover_aug_cols(x)

# S3 method for class 'eigen'
recover_aug_coord(x)

# S3 method for class 'eigen_ord'
as_tbl_ord(x)

# S3 method for class 'eigen_ord'
recover_rows(x)

# S3 method for class 'eigen_ord'
recover_cols(x)

# S3 method for class 'eigen_ord'
recover_inertia(x)

# S3 method for class 'eigen_ord'
recover_coord(x)

# S3 method for class 'eigen_ord'
recover_conference(x)

# S3 method for class 'eigen_ord'
recover_aug_rows(x)

# S3 method for class 'eigen_ord'
recover_aug_cols(x)

# S3 method for class 'eigen_ord'
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

[`base::eigen()`](https://rdrr.io/r/base/eigen.html) usually returns an
object of class `"eigen"`, which contains the numerical
eigendecomposition without annotations such as row and column names. To
facilitate downstream analysis, [`eigen_ord()`](wrap-ord.md) returns a
modified 'eigen' object with row names taken (if available) from the
original data and column names indicating the integer index of each
eigenvector.

## See also

Other methods for eigen-decomposition-based techniques:
[`methods-cmds`](methods-cmds.md),
[`methods-factanal`](methods-factanal.md),
[`methods-princomp`](methods-princomp.md)

Other models from the base package: [`methods-svd`](methods-svd.md)

## Examples

``` r
# eigendecompose covariance matrix of ability tests
gi_eigen <- eigen(ability.cov$cov)

# recover eigenvectors
get_rows(gi_eigen)
#>              EV1         EV2         EV3         EV4         EV5          EV6
#> [1,] -0.22869031 -0.02614951  0.55367919  0.65291077 -0.42125795 -0.191586979
#> [2,] -0.08380792 -0.08365448  0.08175861  0.04179166 -0.21588953  0.964851729
#> [3,] -0.62773627 -0.73872033 -0.12880737 -0.18084446 -0.01592981 -0.103390720
#> [4,] -0.09460078 -0.08807900 -0.01657141  0.54566079  0.81513697  0.144305927
#> [5,] -0.36706353  0.30009148  0.67111717 -0.47373359  0.31556525  0.028394942
#> [6,] -0.63477493  0.59059089 -0.46849833  0.13071747 -0.10793510  0.005954517
identical(get_cols(gi_eigen), get_rows(gi_eigen))
#> [1] TRUE

# wrap as a 'tbl_ord'
as_tbl_ord(gi_eigen)
#> # A tbl_ord of class 'eigen': (6 × 6) · (6 × 6)´
#> # 6 coordinates: EV1, EV2, ..., EV6
#> # Rows (standard, 0%): [ 6 × 6 | 0 ]
#>        EV1     EV2     EV3     EV4     EV5      EV6 | 
#>        [1]     [1]     [1]     [1]     [1]      [1] | 
#>  1 -0.229  -0.0261  0.554   0.653  -0.421  -0.192   | 
#>  2 -0.0838 -0.0837  0.0818  0.0418 -0.216   0.965   | 
#>  3 -0.628  -0.739  -0.129  -0.181  -0.0159 -0.103   | 
#>  4 -0.0946 -0.0881 -0.0166  0.546   0.815   0.144   | 
#>  5 -0.367   0.300   0.671  -0.474   0.316   0.0284  | 
#>  6 -0.635   0.591  -0.468   0.131  -0.108   0.00595 | 
#> # Columns (standard, 0%): [ 6 × 6 | 0 ]
#>        EV1     EV2     EV3     EV4     EV5      EV6 | 
#>        [1]     [1]     [1]     [1]     [1]      [1] | 
#>  1 -0.229  -0.0261  0.554   0.653  -0.421  -0.192   | 
#>  2 -0.0838 -0.0837  0.0818  0.0418 -0.216   0.965   | 
#>  3 -0.628  -0.739  -0.129  -0.181  -0.0159 -0.103   | 
#>  4 -0.0946 -0.0881 -0.0166  0.546   0.815   0.144   | 
#>  5 -0.367   0.300   0.671  -0.474   0.316   0.0284  | 
#>  6 -0.635   0.591  -0.468   0.131  -0.108   0.00595 | 

# same eigendecomposition, preserving names
gi_eigen <- eigen_ord(ability.cov$cov)
#> Warning: `eigen_ord()` is deprecated in favor of `base::eigen()`.
#> This warning is displayed once per session.

# wrap as a 'tbl_ord' and augment with dimension names
augment_ord(as_tbl_ord(gi_eigen))
#> # A tbl_ord of class 'eigen_ord': (6 × 6) · (6 × 6)´
#> # 6 coordinates: EV1, EV2, ..., EV6
#> # Rows (standard, 0%): [ 6 × 6 | 2 ]
#>        EV1     EV2     EV3     EV4     EV5 | name    .element
#>        [1]     [1]     [1]     [1]     [1] | <chr>   <chr>   
#>  1 -0.229  -0.0261  0.554   0.653  -0.421  | general active  
#>  2 -0.0838 -0.0837  0.0818  0.0418 -0.216  | picture active  
#>  3 -0.628  -0.739  -0.129  -0.181  -0.0159 | blocks  active  
#>  4 -0.0946 -0.0881 -0.0166  0.546   0.815  | maze    active  
#>  5 -0.367   0.300   0.671  -0.474   0.316  | reading active  
#>  6 -0.635   0.591  -0.468   0.131  -0.108  | vocab   active  
#> # Columns (standard, 0%): [ 6 × 6 | 2 ]
#>        EV1     EV2     EV3     EV4     EV5 | name    .element
#>        [1]     [1]     [1]     [1]     [1] | <chr>   <chr>   
#>  1 -0.229  -0.0261  0.554   0.653  -0.421  | general active  
#>  2 -0.0838 -0.0837  0.0818  0.0418 -0.216  | picture active  
#>  3 -0.628  -0.739  -0.129  -0.181  -0.0159 | blocks  active  
#>  4 -0.0946 -0.0881 -0.0166  0.546   0.815  | maze    active  
#>  5 -0.367   0.300   0.671  -0.474   0.316  | reading active  
#>  6 -0.635   0.591  -0.468   0.131  -0.108  | vocab   active  

# decomposition returns pure eigenvectors
get_conference(gi_eigen)
#> [1] 0 0
```
