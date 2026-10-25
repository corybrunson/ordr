# Supplement 'tbl_ord' objects with new data

These functions attach supplementary rows or columns to an ordination
object.

## Usage

``` r
recover_supp_rows(x)

# Default S3 method
recover_supp_rows(x)

recover_supp_cols(x)

# Default S3 method
recover_supp_cols(x)
```

## Arguments

- x:

  An object of class '[tbl_ord](tbl_ord.md)'.

## Value

Matrices having the same numbers of columns as returned by
[`recover_rows()`](recoverers.md) and [`recover_cols()`](recoverers.md).

## Details

The `recover_supp_*()` [S3
methods](https://rdrr.io/r/base/UseMethod.html) produce matrices of
supplemental rows or columns of a [tbl_ord](tbl_ord.md) object from the
object itself. The motivating example is linear discriminant analysis,
which produces a natural biplot of class discriminant centroids and
variable axes but is usually supplemented with case discriminant scores.
The supplementary values are augmented with an `.element` column whose
value indicates their source and can be incorporated into a [tidied
form](https://ggplot2.tidyverse.org/reference/fortify.html). If no
supplementary rows of a factor are produced, the functions return
`NULL`.

## See also

Other generic recoverers: [`augmentation`](augmentation.md),
[`conference`](conference.md), [`recoverers`](recoverers.md)
