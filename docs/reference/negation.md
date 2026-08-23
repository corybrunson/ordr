# Negation of ordination axes

Negate the coordinates of a subset of ordination axes in both row and
column singular vectors.

## Usage

``` r
get_negation(x)

revert_negation(x)

negate_ord(x, negation = NULL)

negate_to_first_orthant(x, .matrix)
```

## Arguments

- x:

  A [tbl_ord](tbl_ord.md).

- negation:

  Integer vector of coordinates to negate.

- .matrix:

  A character string partially matched (lowercase) to several indicators
  for one or both matrices in a matrix decomposition used for
  ordination. The standard values are `"rows"`, `"cols"`, and `"dims"`
  (for both).

## Value

`negate_ord()` and `negate_to_first_orthant()` return a tbl_ord with
certain axes negated but the wrapped model unchanged. `get_negation()`
returns the current negations. `revert_negation()` returns the tbl_ord
without any manual negations.

A tbl_ord; the wrapped model is unchanged.

## Details

For purposes of comparison and visualization, it can be useful to negate
the (already artificial) coordinates of an ordination, either by fixed
criteria or to better align with another basis (matrix) of coordinates.
`negate_ord()` allows the user to negate specified coordinates of an
ordination.

`get_negation()` accesses the negations of an ordination, an integer
vector of `1`s and `-1`s stored as a `"negate"` attribute.

## Examples

``` r
(pca <- ordinate(iris, cols = 1:4, prcomp))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 2 ]
#>       PC1     PC2     PC3      PC4 | .element Species
#>     [630] [36.16] [11.65]  [3.551] | <chr>    <fct>  
#> 1 -2.68   -0.319   0.0279  0.00226 | active   setosa 
#> 2 -2.71    0.177   0.210   0.0990  | active   setosa 
#> 3 -2.89    0.145  -0.0179  0.0200  | active   setosa 
#> 4 -2.75    0.318  -0.0316 -0.0756  | active   setosa 
#> 5 -2.73   -0.327  -0.0901 -0.0613  | active   setosa 
#>                 ⋮                           ⋮        
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1     PC2     PC3      PC4 | name    center .element
#>       [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361  -0.657   0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845 -0.730  -0.598  -0.320   | Sepal.…   3.06 active  
#> 3  0.857   0.173  -0.0762 -0.480   | Petal.…   3.76 active  
#> 4  0.358   0.0755 -0.546   0.754   | Petal.…   1.20 active  
ggbiplot(pca) + geom_rows_point() + geom_cols_vector()


# manually negate second coordinate
(pca_neg <- negate_ord(pca, 2))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 2 ]
#>       PC1     PC2     PC3      PC4 | .element Species
#>     [630] [36.16] [11.65]  [3.551] | <chr>    <fct>  
#> 1 -2.68    0.319   0.0279  0.00226 | active   setosa 
#> 2 -2.71   -0.177   0.210   0.0990  | active   setosa 
#> 3 -2.89   -0.145  -0.0179  0.0200  | active   setosa 
#> 4 -2.75   -0.318  -0.0316 -0.0756  | active   setosa 
#> 5 -2.73    0.327  -0.0901 -0.0613  | active   setosa 
#>                 ⋮                           ⋮        
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1     PC2     PC3      PC4 | name    center .element
#>       [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361   0.657   0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845  0.730  -0.598  -0.320   | Sepal.…   3.06 active  
#> 3  0.857  -0.173  -0.0762 -0.480   | Petal.…   3.76 active  
#> 4  0.358  -0.0755 -0.546   0.754   | Petal.…   1.20 active  
ggbiplot(pca_neg) + geom_rows_point() + geom_cols_vector()


# NB: 'prcomp' method takes precedence; negations are part of the wrapper
biplot(pca)

biplot(pca_neg)


# negate to the first orthant
(pca_orth <- negate_to_first_orthant(pca, "v"))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 2 ]
#>       PC1     PC2     PC3      PC4 | .element Species
#>     [630] [36.16] [11.65]  [3.551] | <chr>    <fct>  
#> 1 -2.68    0.319  -0.0279  0.00226 | active   setosa 
#> 2 -2.71   -0.177  -0.210   0.0990  | active   setosa 
#> 3 -2.89   -0.145   0.0179  0.0200  | active   setosa 
#> 4 -2.75   -0.318   0.0316 -0.0756  | active   setosa 
#> 5 -2.73    0.327   0.0901 -0.0613  | active   setosa 
#>                 ⋮                           ⋮        
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1     PC2     PC3      PC4 | name    center .element
#>       [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361   0.657  -0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845  0.730   0.598  -0.320   | Sepal.…   3.06 active  
#> 3  0.857  -0.173   0.0762 -0.480   | Petal.…   3.76 active  
#> 4  0.358  -0.0755  0.546   0.754   | Petal.…   1.20 active  
get_negation(pca_orth)
#> PC1 PC2 PC3 PC4 
#>   1  -1  -1   1 
```
