# Confer inertia to factors of a 'tbl_ord' object

Re-distribute inertia between rows and columns in an ordination.

## Usage

``` r
recover_conference(x)

# Default S3 method
recover_conference(x)

get_conference(x)

revert_conference(x)

confer_inertia(x, p)
```

## Arguments

- x:

  A [tbl_ord](tbl_ord.md).

- p:

  Numeric vector of length 1 or 2. If length 1, the proportion of the
  inertia assigned to the cases, with the remainder `1 - p` assigned to
  the variables. If length 2, the proportions of the inertia assigned to
  the cases and to the variables, respectively.

## Value

`recover_conference()` returns the (statically implemented) distribution
of inertia between the rows and the columns as stored in the model.
`confer_inertia()` returns a tbl_ord with a specified distribution of
inertia but the wrapped model unchanged. `get_conference()` returns the
distribution currently conferred.

## Details

The *inertia* of a singular value decomposition \\X=UDV'\\ consists in
the squares of the singular values (the diagonal elements of \\D\\), and
represents the variance, likened to the physical inertia, in the
directions of the orthogonal singular vectors (the columns of \\U\\ or
of \\V\\). Biplots superimpose the projections of the rows and the
columns of \\X\\ onto these coordinate vectors, scaled by some
proportion of the total inertia: \\UD^p\\ and \\VD^q\\. A biplot is
*balanced* if \\p+q=1\\. Read Orlov (2013) for more on conferring
inertia in PCA.

`recover_conference()`, like the other recoverers, is an [S3
method](https://rdrr.io/r/base/UseMethod.html) that is exported for
convenience but not intended to be used directly.

*Note: In case the `"inertia"` attribute is a rectangular matrix, one
may only be able to confer it entirely to the cases (`p = 1`) or
entirely to the variables (`p = 0`).*

## References

Orlov K (2013) *Answer to* "Algebra of LDA. Fisher discrimination power
of a variable and Linear Discriminant Analysis". CrossValidated,
accessed 2019-07-26. <https://stats.stackexchange.com/a/83114/68743>

## See also

Other generic recoverers: [`augmentation`](augmentation.md),
[`recoverers`](recoverers.md), [`supplementation`](supplementation.md)

## Examples

``` r
# illustrative ordination: correspendence analysis of hair & eye data
haireye_ca <- ordinate(
  as.data.frame(rowSums(HairEyeColor, dims = 2L)),
  cols = everything(), model = MASS::corresp
)
print(haireye_ca)
#> # A tbl_ord of class 'correspondence': (4 × 1) · (4 × 1)´
#> # 1 coordinate: Can1
#> # Rows (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.10  | Black active  
#> 2 -0.324 | Brown active  
#> 3 -0.283 | Red   active  
#> 4  1.83  | Blond active  
#> # Columns (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.08  | Brown active  
#> 2  1.20  | Blue  active  
#> 3 -0.465 | Hazel active  
#> 4  0.354 | Green active  

# check distribution of inertia
get_conference(haireye_ca)
#> [1] 0 0
# confer inertia to rows, then to columns
confer_inertia(haireye_ca, "rows")
#> # A tbl_ord of class 'correspondence': (4 × 1) · (4 × 1)´
#> # 1 coordinate: Can1
#> # Rows (principal, 100%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#> [0.2088] | <chr> <chr>   
#> 1 -0.505 | Black active  
#> 2 -0.148 | Brown active  
#> 3 -0.130 | Red   active  
#> 4  0.835 | Blond active  
#> # Columns (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.08  | Brown active  
#> 2  1.20  | Blue  active  
#> 3 -0.465 | Hazel active  
#> 4  0.354 | Green active  
confer_inertia(haireye_ca, "columns")
#> # A tbl_ord of class 'correspondence': (4 × 1) · (4 × 1)´
#> # 1 coordinate: Can1
#> # Rows (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.10  | Black active  
#> 2 -0.324 | Brown active  
#> 3 -0.283 | Red   active  
#> 4  1.83  | Blond active  
#> # Columns (principal, 100%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#> [0.2088] | <chr> <chr>   
#> 1 -0.492 | Brown active  
#> 2  0.547 | Blue  active  
#> 3 -0.213 | Hazel active  
#> 4  0.162 | Green active  
# confer inertia symmetrically
(haireye_ca <- confer_inertia(haireye_ca, "symmetric"))
#> # A tbl_ord of class 'correspondence': (4 × 1) · (4 × 1)´
#> # 1 coordinate: Can1
#> # Rows (symmetric, 50%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#> [0.4569] | <chr> <chr>   
#> 1 -0.746 | Black active  
#> 2 -0.219 | Brown active  
#> 3 -0.192 | Red   active  
#> 4  1.24  | Blond active  
#> # Columns (symmetric, 50%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#> [0.4569] | <chr> <chr>   
#> 1 -0.728 | Brown active  
#> 2  0.810 | Blue  active  
#> 3 -0.315 | Hazel active  
#> 4  0.239 | Green active  
# check redistributed inertia
get_conference(haireye_ca)
#> [1] 0.5 0.5
# restore default distribution of inertia
revert_conference(haireye_ca)
#> # A tbl_ord of class 'correspondence': (4 × 1) · (4 × 1)´
#> # 1 coordinate: Can1
#> # Rows (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.10  | Black active  
#> 2 -0.324 | Brown active  
#> 3 -0.283 | Red   active  
#> 4  1.83  | Blond active  
#> # Columns (standard, 0%): [ 4 × 1 | 2 ]
#>     Can1 | name  .element
#>      [1] | <chr> <chr>   
#> 1 -1.08  | Brown active  
#> 2  1.20  | Blue  active  
#> 3 -0.465 | Hazel active  
#> 4  0.354 | Green active  
```
