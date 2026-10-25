# Access factors, coordinates, and metadata from ordination objects

These functions return information about the matrix factorization
underlying an ordination.

## Usage

``` r
recover_rows(x)

recover_cols(x)

# Default S3 method
recover_rows(x)

# Default S3 method
recover_cols(x)

# S3 method for class 'data.frame'
recover_rows(x)

# S3 method for class 'data.frame'
recover_cols(x)

get_rows(x, elements = "all")

get_cols(x, elements = "all")

# S3 method for class 'tbl_ord'
as.matrix(x, ..., .matrix, elements = "all")

recover_inertia(x)

# Default S3 method
recover_inertia(x)

recover_coord(x)

# Default S3 method
recover_coord(x)

# S3 method for class 'data.frame'
recover_coord(x)

get_coord(x)

get_inertia(x)

# S3 method for class 'tbl_ord'
dim(x)
```

## Arguments

- x:

  An object of class '[tbl_ord](tbl_ord.md)'.

- elements:

  Character vector; which elements of each factor for which to render
  graphical elements. One of `"active"` (the default) or any
  supplementary element type defined by the specific class methods (e.g.
  `"score"` for 'factanal', 'lda_ord', and 'cancord_ord' and
  `"intraset"` and `"interset"` for 'cancor_ord'), via [partial
  matching](https://rdrr.io/r/base/match.arg.html).

- ...:

  Additional arguments from
  [`base::as.matrix()`](https://rdrr.io/r/base/matrix.html); ignored.

- .matrix:

  A character string partially matched (lowercase) to several indicators
  for one or both matrices in a matrix decomposition used for
  ordination. The standard values are `"rows"`, `"cols"`, and `"dims"`
  (for both).

## Value

The `recover_*()` functions are generics whose methods return base R
objects retrieved from the model wrapped in the 'tbl_ord' class:

- `rows`: the row matrix as stored in the model

- `cols`: the column matrix as stored in the model

- `inertia`: the vector of eigen-values or squared singular values,
  often known by other names depending on the model

- `coord`: names for the artificial axes, from the model if available
  The `get_*()` functions (which are not generics) return modifications
  of these objects:

- `rows`: the recovered rows, adjusted according to any negation of axes
  or conference of inertia

- `cols`: the recovered columns, adjusted according to any negation of
  axes or conference of inertia

- `inertia`: the recovered inertia, named by the recovered coordinates

- `coord`: the recovered coordinates (unmodified)
  [`dim()`](https://rdrr.io/r/base/dim.html) returns the dimensions of
  the decomposed matrix, i.e. the numbers of rows of `recover_rows()`
  and of `recover_cols()`.

## Details

The `recover_*()` [S3 methods](https://rdrr.io/r/base/UseMethod.html)
extract one or both of the row and column matrix factors that constitute
the original ordination. These are interpreted as the case scores (rows)
and the variable loadings (columns). The `get_*()` functions optionally
(and by default) include any supplemental observations (see
[supplementation](supplementation.md)).

The `recover_*()` functions are generics that require methods for each
ordination class. They are not intended to be called directly but are
exported so that users can query `methods("recover_*")`.

`get_coord()` retrieves the names of the coordinates shared by the
matrix factors on which the original data were ordinated, and
`get_inertia()` retrieves a vector of the inertia with these names.
[`dim()`](https://rdrr.io/r/base/dim.html) retrieves the dimensions of
the row and column factors, which reflect the dimensions of the matrix
they reconstruct—**not** the original data matrix. (This matters for
techniques that rely on eigendecomposition, for which the decomposed
matrix is square.)

## See also

Other generic recoverers: [`augmentation`](augmentation.md),
[`conference`](conference.md), [`supplementation`](supplementation.md)

## Examples

``` r
# example ordination: LRA of U.S. arrests data
arrests_lra <- ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)

# extract matrix factors
as.matrix(arrests_lra, .matrix = "rows")
#>                      LRSV1        LRSV2
#> Alabama        -0.68001198  0.929601139
#> Alaska          0.92998988 -0.624577164
#> Arizona        -0.32984955 -1.311581695
#> Arkansas       -0.35134428  0.277323143
#> California      0.55165901 -1.004280086
#> Colorado        1.22910659 -0.638846890
#> Connecticut    -0.43610804 -1.027017532
#> Delaware       -1.54269183 -1.349322890
#> Florida        -0.54682514  0.300298438
#> Georgia         0.15812112  1.916932033
#> Hawaii          3.51365819  2.150284964
#> Idaho          -0.05250833 -2.109188891
#> Illinois       -0.52429773  0.002652531
#> Indiana         1.22231804  0.881075829
#> Iowa            1.39095549 -0.632985189
#> Kansas          0.75715595  0.380674137
#> Kentucky        0.68864901  2.024832534
#> Louisiana      -0.69015525  1.238509473
#> Maine          -0.63716612 -1.499305459
#> Maryland       -0.63629812 -0.286175559
#> Massachusetts  -0.22762978 -1.124006923
#> Michigan        0.41414730  0.171745261
#> Minnesota       1.45222388 -0.789191960
#> Mississippi    -1.47068651  1.436924829
#> Missouri        0.78566710  0.278289912
#> Montana         0.65946921  0.566956397
#> Nebraska        0.82281297 -0.284750183
#> Nevada          1.14994121  0.060475841
#> New Hampshire   0.88778358 -0.710870888
#> New Jersey      0.01717089  0.205735676
#> New Mexico     -0.12336523 -0.223126918
#> New York       -0.35365080  0.100656157
#> North Carolina -2.35659680  0.189728037
#> North Dakota    0.74912231 -2.900272794
#> Ohio            1.11076020  0.766013515
#> Oklahoma        0.30637650 -0.053491161
#> Oregon          1.13217636 -1.312593507
#> Pennsylvania    0.48983869  0.840580871
#> Rhode Island   -2.42442178 -1.868251942
#> South Carolina -0.96800438  0.752042083
#> South Dakota    0.61067278 -0.091684230
#> Tennessee       0.55166149  1.334277800
#> Texas           0.22895595  1.088292623
#> Utah            1.20949943 -1.771844197
#> Vermont         1.78310111 -0.254863377
#> Virginia        0.33180838  0.612645656
#> Washington      1.07067103 -1.635935144
#> West Virginia  -0.02043499  1.475198849
#> Wisconsin       1.43728882  0.033416687
#> Wyoming        -0.50955934  0.033243165
as.matrix(arrests_lra, .matrix = "cols")
#>             LRSV1      LRSV2
#> Murder   0.283086  4.9570302
#> Rape     2.876702 -0.3660163
#> Assault -0.370595 -0.1805698
# special named functions
get_rows(arrests_lra)
#>                      LRSV1        LRSV2
#> Alabama        -0.68001198  0.929601139
#> Alaska          0.92998988 -0.624577164
#> Arizona        -0.32984955 -1.311581695
#> Arkansas       -0.35134428  0.277323143
#> California      0.55165901 -1.004280086
#> Colorado        1.22910659 -0.638846890
#> Connecticut    -0.43610804 -1.027017532
#> Delaware       -1.54269183 -1.349322890
#> Florida        -0.54682514  0.300298438
#> Georgia         0.15812112  1.916932033
#> Hawaii          3.51365819  2.150284964
#> Idaho          -0.05250833 -2.109188891
#> Illinois       -0.52429773  0.002652531
#> Indiana         1.22231804  0.881075829
#> Iowa            1.39095549 -0.632985189
#> Kansas          0.75715595  0.380674137
#> Kentucky        0.68864901  2.024832534
#> Louisiana      -0.69015525  1.238509473
#> Maine          -0.63716612 -1.499305459
#> Maryland       -0.63629812 -0.286175559
#> Massachusetts  -0.22762978 -1.124006923
#> Michigan        0.41414730  0.171745261
#> Minnesota       1.45222388 -0.789191960
#> Mississippi    -1.47068651  1.436924829
#> Missouri        0.78566710  0.278289912
#> Montana         0.65946921  0.566956397
#> Nebraska        0.82281297 -0.284750183
#> Nevada          1.14994121  0.060475841
#> New Hampshire   0.88778358 -0.710870888
#> New Jersey      0.01717089  0.205735676
#> New Mexico     -0.12336523 -0.223126918
#> New York       -0.35365080  0.100656157
#> North Carolina -2.35659680  0.189728037
#> North Dakota    0.74912231 -2.900272794
#> Ohio            1.11076020  0.766013515
#> Oklahoma        0.30637650 -0.053491161
#> Oregon          1.13217636 -1.312593507
#> Pennsylvania    0.48983869  0.840580871
#> Rhode Island   -2.42442178 -1.868251942
#> South Carolina -0.96800438  0.752042083
#> South Dakota    0.61067278 -0.091684230
#> Tennessee       0.55166149  1.334277800
#> Texas           0.22895595  1.088292623
#> Utah            1.20949943 -1.771844197
#> Vermont         1.78310111 -0.254863377
#> Virginia        0.33180838  0.612645656
#> Washington      1.07067103 -1.635935144
#> West Virginia  -0.02043499  1.475198849
#> Wisconsin       1.43728882  0.033416687
#> Wyoming        -0.50955934  0.033243165
get_cols(arrests_lra)
#>             LRSV1      LRSV2
#> Murder   0.283086  4.9570302
#> Rape     2.876702 -0.3660163
#> Assault -0.370595 -0.1805698
# get dimensions of underlying matrix factorization (not of original data)
dim(arrests_lra)
#> [1] 50  3

# get names of artificial / latent coordinates
get_coord(arrests_lra)
#> [1] "LRSV1" "LRSV2"
# get distribution of inertia
get_inertia(arrests_lra)
#>       LRSV1       LRSV2 
#> 0.013826903 0.004074913 
```
