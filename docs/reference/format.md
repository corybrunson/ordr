# Format a tbl_ord for printing

These methods of [`base::format()`](https://rdrr.io/r/base/format.html)
and [`base::print()`](https://rdrr.io/r/base/print.html) render a
(usually more) tidy readout of a [tbl_ord](tbl_ord.md) that is
consistent across all original ordination classes.

## Usage

``` r
# S3 method for class 'tbl_ord'
format(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'tbl_ord'
print(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)
```

## Arguments

- x:

  A [tbl_ord](tbl_ord.md).

- width:

  Width of text output to generate. This defaults to `NULL`, which means
  use the `width`
  [option](https://pillar.r-lib.org/reference/pillar_options.html).

- ...:

  Additional arguments.

- n:

  Number(s) of rows to show from each matrix factor, handled as by
  [`tibble::format.tbl()`](https://tibble.tidyverse.org/reference/formatting.html).
  If length 1, will apply to both matrix factors. To pass `NULL` to only
  one factor, be sure to pass as a list, e.g. `n = list(6, NULL)`.

- max_extra_cols, max_footer_lines:

  As in
  [`tibble::format.tbl`](https://tibble.tidyverse.org/reference/formatting.html),
  applied to each matrix factor separately.

## Value

The `format()` method returns a vector of strings that are more
elegantly printed by the [`print()`](https://rdrr.io/r/base/print.html)
method, which itself returns the tbl_ord invisibly.

## Details

The [`base::format()`](https://rdrr.io/r/base/format.html) and
[`base::print()`](https://rdrr.io/r/base/print.html) methods for class
'tbl_ord' are adapted from those for class
'[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)' and
for class 'tbl_graph' from the **tidygraph** package.

**NB:** The `format.tbl_ord()` method is tedious but cannot be easily
modularized without invoking [recoverers](recoverers.md),
[annotation](annotation.md), and [augmentation](augmentation.md)
multiple times, thereby significantly reducing performance. It calls
upon **pillar** for `tbl_df` formatting then revises the results per
`tbl_ord`.

## Examples

``` r
iris_pca <- ordinate(iris[1:4], prcomp)

# single value applies to both factors
print(iris_pca, n = 2)
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>       PC1    PC2     PC3      PC4 | .element
#>     [630][36.16] [11.65]  [3.551] | <chr>   
#> 1 -2.68   -0.319  0.0279  0.00226 | active  
#> 2 -2.71    0.177  0.210   0.0990  | active  
#>                 ⋮                      ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1    PC2     PC3      PC4 | name    center .element
#>       [1]    [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361  -0.657  0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845 -0.730 -0.598  -0.320   | Sepal.…   3.06 active  
#>                 ⋮                              ⋮           
print(iris_pca, n = 10)
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>        PC1     PC2     PC3      PC4 | .element
#>      [630] [36.16] [11.65]  [3.551] | <chr>   
#>  1 -2.68   -0.319   0.0279  0.00226 | active  
#>  2 -2.71    0.177   0.210   0.0990  | active  
#>  3 -2.89    0.145  -0.0179  0.0200  | active  
#>  4 -2.75    0.318  -0.0316 -0.0756  | active  
#>  5 -2.73   -0.327  -0.0901 -0.0613  | active  
#>  6 -2.28   -0.741  -0.169  -0.0242  | active  
#>  7 -2.82    0.0895 -0.258  -0.0481  | active  
#>  8 -2.63   -0.163   0.0219 -0.0453  | active  
#>  9 -2.89    0.578  -0.0208 -0.0267  | active  
#> 10 -2.67    0.114   0.198  -0.0563  | active  
#>                  ⋮                       ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>        PC1     PC2     PC3      PC4 | name    center .element
#>        [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   
#>  1  0.361  -0.657   0.582   0.315   | Sepal.…   5.84 active  
#>  2 -0.0845 -0.730  -0.598  -0.320   | Sepal.…   3.06 active  
#>  3  0.857   0.173  -0.0762 -0.480   | Petal.…   3.76 active  
#>  4  0.358   0.0755 -0.546   0.754   | Petal.…   1.20 active  

# double values apply to factors in order
print(iris_pca, n = c(6, 2))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>       PC1    PC2     PC3      PC4 | .element
#>     [630][36.16] [11.65]  [3.551] | <chr>   
#> 1 -2.68   -0.319  0.0279  0.00226 | active  
#> 2 -2.71    0.177  0.210   0.0990  | active  
#> 3 -2.89    0.145 -0.0179  0.0200  | active  
#> 4 -2.75    0.318 -0.0316 -0.0756  | active  
#> 5 -2.73   -0.327 -0.0901 -0.0613  | active  
#> 6 -2.28   -0.741 -0.169  -0.0242  | active  
#>                 ⋮                      ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1    PC2     PC3      PC4 | name    center .element
#>       [1]    [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361  -0.657  0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845 -0.730 -0.598  -0.320   | Sepal.…   3.06 active  
#>                 ⋮                              ⋮           

# use `list()` to pass `NULL` (for default) to only one factor
print(iris_pca, n = list(2, NULL))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>       PC1     PC2     PC3      PC4 | .element
#>     [630] [36.16] [11.65]  [3.551] | <chr>   
#> 1 -2.68   -0.319   0.0279  0.00226 | active  
#> 2 -2.71    0.177   0.210   0.0990  | active  
#>                 ⋮                       ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1     PC2     PC3      PC4 | name    center .element
#>       [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361  -0.657   0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845 -0.730  -0.598  -0.320   | Sepal.…   3.06 active  
#> 3  0.857   0.173  -0.0762 -0.480   | Petal.…   3.76 active  
#> 4  0.358   0.0755 -0.546   0.754   | Petal.…   1.20 active  
print(iris_pca, n = list(NULL, 2))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 1 ]
#>       PC1    PC2     PC3      PC4 | .element
#>     [630][36.16] [11.65]  [3.551] | <chr>   
#> 1 -2.68   -0.319  0.0279  0.00226 | active  
#> 2 -2.71    0.177  0.210   0.0990  | active  
#> 3 -2.89    0.145 -0.0179  0.0200  | active  
#> 4 -2.75    0.318 -0.0316 -0.0756  | active  
#> 5 -2.73   -0.327 -0.0901 -0.0613  | active  
#>                 ⋮                      ⋮    
#> # Columns (standard, 0%): [ 4 × 4 | 3 ]
#>       PC1    PC2     PC3      PC4 | name    center .element
#>       [1]    [1]     [1]      [1] | <chr>    <dbl> <chr>   
#> 1  0.361  -0.657  0.582   0.315   | Sepal.…   5.84 active  
#> 2 -0.0845 -0.730 -0.598  -0.320   | Sepal.…   3.06 active  
#>                 ⋮                              ⋮           

# too narrow width for all coordinates
print(iris_pca, width = 22)
#> # tbl_ord: (150×4)·(4×4)´
#> # 4 coordinates
#> # Rows: [150×4|1]
#>       PC1 … | 
#>     [630]   | 
#> 1 -2.68     | 
#> 2 -2.71     | 
#> 3 -2.89   … | 
#> 4 -2.75     | 
#> 5 -2.73     | 
#>     ⋮       
#> # Columns: [4×4|3]
#>       PC1 … | 
#>       [1]   | 
#> 1  0.361    | 
#> 2 -0.0845 … | 
#> 3  0.857    | 
#> 4  0.358    | 

iris_lda <- ordinate(iris[1:4], lda_ord, grouping = iris$Species)

# supplementary elements appear below active elements
print(iris_lda)
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

# too many annotations to print within console width
print(iris_lda, width = 40)
#> # A tbl_ord <lda>: (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal): [ 153 × 2 | 5 ]
#>      LD1     LD2 | name      
#>   [2366] [20.98] | <chr>     
#> 1  7.61  -0.215  | setosa    
#> 2 -1.83   0.728  | versicolor
#> 3 -5.78  -0.513  | virginica 
#> 4  8.06  -0.300  | NA        
#> 5  7.13   0.787  | NA        
#>        ⋮               ⋮     
#> # ℹ 4 more
#> #   variables:
#> #   prior <dbl>,
#> #   counts <int>,
#> #   grouping <chr>,
#> #   .element <chr>
#> # Columns (standard): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name       
#>      [1]     [1] | <chr>      
#> 1  0.829 -0.0241 | Sepal.Leng…
#> 2  1.53  -2.16   | Sepal.Width
#> 3 -2.20   0.932  | Petal.Leng…
#> 4 -2.81  -2.84   | Petal.Width
#> # ℹ 1 more
#> #   variable:
#> #   .element <chr>

# annotations too wide to print are summarized in footers
print(iris_pca, width = 30)
#> # tbl_ord: (150×4)·(4×4)´
#> # 4 coordinates
#> # Rows (100%): [ 150 × 4 | 1 ]
#>       PC1 … | .element
#>     [630]   | <chr>   
#> 1 -2.68     | active  
#> 2 -2.71     | active  
#> 3 -2.89   … | active  
#> 4 -2.75     | active  
#> 5 -2.73     | active  
#>     ⋮          ⋮    
#> # Columns (0%): [ 4 × 4 | 3 ]
#>       PC1 … | name     
#>       [1]   | <chr>    
#> 1  0.361    | Sepal.Le…
#> 2 -0.0845 … | Sepal.Wi…
#> 3  0.857    | Petal.Le…
#> 4  0.358    | Petal.Wi…
#> # ℹ 2 more
#> #   variables:
#> #   center <dbl>,
#> #   .element <chr>

# cap the number of lines of each factor's footer note
print(iris_pca, width = 30, max_footer_lines = 2)
#> # tbl_ord: (150×4)·(4×4)´
#> # 4 coordinates
#> # Rows (100%): [ 150 × 4 | 1 ]
#>       PC1 … | .element
#>     [630]   | <chr>   
#> 1 -2.68     | active  
#> 2 -2.71     | active  
#> 3 -2.89   … | active  
#> 4 -2.75     | active  
#> 5 -2.73     | active  
#>     ⋮          ⋮    
#> # Columns (0%): [ 4 × 4 | 3 ]
#>       PC1 … | name     
#>       [1]   | <chr>    
#> 1  0.361    | Sepal.Le…
#> 2 -0.0845 … | Sepal.Wi…
#> 3  0.857    | Petal.Le…
#> 4  0.358    | Petal.Wi…
#> # ℹ 2 more
#> #   variables: …

haireye_ca <- ordinate(
  as.data.frame(rowSums(HairEyeColor, dims = 2L)),
  cols = everything(), model = MASS::corresp, nf = 3
)

# default conference: standard coordinates for both factors
print(haireye_ca)
#> # A tbl_ord of class 'correspondence': (4 × 3) · (4 × 3)´
#> # 3 coordinates: Can1, Can2, Can3
#> # Rows (standard, 0%): [ 4 × 3 | 2 ]
#>     Can1   Can2    Can3 | name  .element
#>      [1]    [1]     [1] | <chr> <chr>   
#> 1 -1.10   1.44  -1.09   | Black active  
#> 2 -0.324 -0.219  0.957  | Brown active  
#> 3 -0.283 -2.14  -1.63   | Red   active  
#> 4  1.83   0.467 -0.318  | Blond active  
#> # Columns (standard, 0%): [ 4 × 3 | 2 ]
#>     Can1   Can2    Can3 | name  .element
#>      [1]    [1]     [1] | <chr> <chr>   
#> 1 -1.08   0.592 -0.424  | Brown active  
#> 2  1.20   0.556  0.0924 | Blue  active  
#> 3 -0.465 -1.12   1.97   | Hazel active  
#> 4  0.354 -2.27  -1.72   | Green active  

# column-principal coordinates, very narrow width
print(confer_inertia(haireye_ca, "cols"), width = 18)
#> # tbl_ord: (4×3)·(4×3)´
#> # 3 coordinates
#> # Rows: [4×3|2]
#>     Can1 … | 
#>      [1]   | 
#> 1 -1.10    | 
#> 2 -0.324 … | 
#> 3 -0.283   | 
#> 4  1.83    | 
#> # Columns: [4×3|2]
#>     Can1 … | 
#> [0.2088]   | 
#> 1 -0.492   | 
#> 2  0.547 … | 
#> 3 -0.213   | 
#> 4  0.162   | 

# symmetric coordinates
print(confer_inertia(haireye_ca, "symmetric"), max_extra_cols = 1)
#> # A tbl_ord of class 'correspondence': (4 × 3) · (4 × 3)´
#> # 3 coordinates: Can1, Can2, Can3
#> # Rows (symmetric, 50%): [ 4 × 3 | 2 ]
#>     Can1    Can2    Can3 | name  .element
#> [0.4569][0.1491] [0.051] | <chr> <chr>   
#> 1 -0.746  0.556  -0.246  | Black active  
#> 2 -0.219 -0.0846  0.216  | Brown active  
#> 3 -0.192 -0.828  -0.368  | Red   active  
#> 4  1.24   0.180  -0.0718 | Blond active  
#> # Columns (symmetric, 50%): [ 4 × 3 | 2 ]
#>     Can1    Can2    Can3 | name  .element
#> [0.4569][0.1491] [0.051] | <chr> <chr>   
#> 1 -0.728  0.229  -0.0957 | Brown active  
#> 2  0.810  0.215   0.0209 | Blue  active  
#> 3 -0.315 -0.434   0.445  | Hazel active  
#> 4  0.239 -0.878  -0.388  | Green active  
```
