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

  An object of class '\[tbl_ord\]'.

## Value

Matrices having the same numbers of columns as returned by
\[recover_rows()\] and \[recover_cols()\].

## Details

The \`recover_supp\_\*()\` \[S3 methods\]\[base::S3Methods\] produce
matrices of supplemental rows or columns of a \[tbl_ord\] object from
the object itself. The motivating example is linear discriminant
analysis, which produces a natural biplot of class discriminant
centroids and variable axes but is usually supplemented with case
discriminant scores. The supplementary values are augmented with an
\`.element\` column whose value indicates their source and can be
incorporated into a \[tidied form\]\[ggplot2::fortify\]. If no
supplementary rows of a factor are produced, the functions return
\`NULL\`.

## See also

Other generic recoverers: [`augmentation`](augmentation.md),
[`conference`](conference.md), [`recoverers`](recoverers.md)
