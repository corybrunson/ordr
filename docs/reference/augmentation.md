# Augment factors and coordinates of 'tbl_ord' objects

These functions return data associated with the cases, variables, and
coordinates of an ordination object, and attach it to the object.

## Usage

``` r
recover_aug_rows(x)

recover_aug_cols(x)

recover_aug_coord(x)

augment_ord(x, .matrix = "dims")
```

## Arguments

- x:

  An object of class '[tbl_ord](tbl_ord.md)'.

- .matrix:

  A character string partially matched (lowercase) to several indicators
  for one or both matrices in a matrix decomposition used for
  ordination. The standard values are `"rows"`, `"cols"`, and `"dims"`
  (for both).

## Value

The `recover_aug_*()` functions return
[tibble](https://tibble.tidyverse.org/reference/tibble.html)s having the
same numbers of rows as `recover_*()`. `augment_ord()` returns an
augmented tbl_ord with the wrapped model unchanged.

## Details

The `recover_aug_*()` [S3
methods](https://rdrr.io/r/base/UseMethod.html) produce
[tibble](https://tibble.tidyverse.org/reference/tibble.html)s of values
associated with the rows, columns, and artificial coordinates of an
object of class '[tbl_ord](tbl_ord.md)'. The first field of each tibble
is `name`, which contains the row, column, or coordinate names.
Additional fields contain information about the rows, columns, or
coordinates extracted from the ordination object.

The function `augment_ord()` returns the ordination with either or both
matrix factors annotated with the result of `recover_aug_*()`. In this
way `augment_ord()` works like
[`generics::augment()`](https://generics.r-lib.org/reference/augment.html),
as popularized by the **broom** package, by extracting information about
the rows and columns, but it differs in returning an annotated 'tbl_ord'
rather than a
['tbl_df'](https://tibble.tidyverse.org/reference/tbl_df-class.html)
object. The advantage of implementing separate methods for the rows,
columns, and artificial coordinates is that more information contained
in the original object becomes accessible to the user.

## See also

[tidiers](tidiers.md) and [annotation](annotation.md) methods that
interface with augmentation.

Other generic recoverers: [`conference`](conference.md),
[`recoverers`](recoverers.md), [`supplementation`](supplementation.md)
