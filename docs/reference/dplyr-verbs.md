# **dplyr** verbs for tbl_ord factors

These functions adapt
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html) verbs
to the factors of a [tbl_ord](tbl_ord.md).

The raw verbs are not defined for tbl_ords; instead, each verb has two
analogues, corresponding to the two matrix factors. They each rely on a
common workhorse function, which takes the composition of the **dplyr**
verb with `annotation_*`, applied to the factor, removes any variables
corresponding to coordinates or already annotated, and only then assigns
it as the new `"*_annotation"` attribute of `.data` (see
[annotation](annotation.md)). Note that these functions are not generics
and so cannot be extended to other classes.

## Usage

``` r
pull_factor(.data, var = -1, .matrix)

pull_rows(.data, var = -1)

pull_cols(.data, var = -1)

rename_rows(.data, ...)

rename_cols(.data, ...)

select_rows(.data, ...)

select_cols(.data, ...)

mutate_rows(.data, ...)

mutate_cols(.data, ...)

transmute_rows(.data, ...)

transmute_cols(.data, ...)

cbind_rows(.data, ..., elements = "all")

cbind_cols(.data, ..., elements = "all")

left_join_rows(.data, ...)

left_join_cols(.data, ...)
```

## Arguments

- .data:

  An object of class '[tbl_ord](tbl_ord.md)'.

- var:

  A variable specified as in
  [`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html).

- .matrix:

  A character string partially matched (lowercase) to several indicators
  for one or both matrices in a matrix decomposition used for
  ordination. The standard values are `"rows"`, `"cols"`, and `"dims"`
  (for both).

- ...:

  Comma-separated unquoted expressions as in, e.g.,
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

- elements:

  Character vector; which elements of each factor for which to render
  graphical elements. One of `"active"` (the default) or any
  supplementary element type defined by the specific class methods (e.g.
  `"score"` for 'factanal', 'lda_ord', and 'cancord_ord' and
  `"intraset"` and `"interset"` for 'cancor_ord'), via [partial
  matching](https://rdrr.io/r/base/match.arg.html).

## Value

A tbl_ord; the wrapped model is unchanged.

## Examples

``` r
# illustrative ordination: LDA of iris data
(iris_lda <- ordinate(iris, cols = 1:4, lda_ord, grouping = iris$Species))
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | name        prior counts grouping  .element Species
#>   [2366] [20.98] | <chr>       <dbl>  <int> <chr>     <chr>    <fct>  
#> 1  7.61  -0.215  | setosa      0.333     50 setosa    active   NA     
#> 2 -1.83   0.728  | versicolor  0.333     50 versicol… active   NA     
#> 3 -5.78  -0.513  | virginica   0.333     50 virginica active   NA     
#> 4  8.06  -0.300  | NA         NA         NA setosa    score    setosa 
#> 5  7.13   0.787  | NA         NA         NA setosa    score    setosa 
#>        ⋮                                    ⋮                         
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name         .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  

# extract a coordinate or annotation
head(pull_rows(iris_lda, Species))
#> [1] <NA>   <NA>   <NA>   setosa setosa setosa
#> Levels: setosa versicolor virginica
pull_cols(iris_lda, LD2)
#> [1] -0.02410215 -2.16452123  0.93192121 -2.83918785

# rename an annotation
rename_cols(iris_lda, species = name)
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | name        prior counts grouping  .element Species
#>   [2366] [20.98] | <chr>       <dbl>  <int> <chr>     <chr>    <fct>  
#> 1  7.61  -0.215  | setosa      0.333     50 setosa    active   NA     
#> 2 -1.83   0.728  | versicolor  0.333     50 versicol… active   NA     
#> 3 -5.78  -0.513  | virginica   0.333     50 virginica active   NA     
#> 4  8.06  -0.300  | NA         NA         NA setosa    score    setosa 
#> 5  7.13   0.787  | NA         NA         NA setosa    score    setosa 
#>        ⋮                                    ⋮                         
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | species      .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  

# select annotations
select_rows(iris_lda, species = name, .element)
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 2 ]
#>      LD1     LD2 | species    .element
#>   [2366] [20.98] | <chr>      <chr>   
#> 1  7.61  -0.215  | setosa     active  
#> 2 -1.83   0.728  | versicolor active  
#> 3 -5.78  -0.513  | virginica  active  
#> 4  8.06  -0.300  | NA         score   
#> 5  7.13   0.787  | NA         score   
#>        ⋮                    ⋮         
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name         .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  

# create, modify, and delete annotations
mutate_cols(iris_lda, vec.length = sqrt(LD1^2 + LD2^2))
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | name        prior counts grouping  .element Species
#>   [2366] [20.98] | <chr>       <dbl>  <int> <chr>     <chr>    <fct>  
#> 1  7.61  -0.215  | setosa      0.333     50 setosa    active   NA     
#> 2 -1.83   0.728  | versicolor  0.333     50 versicol… active   NA     
#> 3 -5.78  -0.513  | virginica   0.333     50 virginica active   NA     
#> 4  8.06  -0.300  | NA         NA         NA setosa    score    setosa 
#> 5  7.13   0.787  | NA         NA         NA setosa    score    setosa 
#>        ⋮                                    ⋮                         
#> # Columns (standard, 0%): [ 4 × 2 | 3 ]
#>      LD1     LD2 | name         .element vec.length
#>      [1]     [1] | <chr>        <chr>         <dbl>
#> 1  0.829 -0.0241 | Sepal.Length active        0.830
#> 2  1.53  -2.16   | Sepal.Width  active        2.65 
#> 3 -2.20   0.932  | Petal.Length active        2.39 
#> 4 -2.81  -2.84   | Petal.Width  active        3.99 
transmute_cols(iris_lda, vec.length = sqrt(LD1^2 + LD2^2))
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | name        prior counts grouping  .element Species
#>   [2366] [20.98] | <chr>       <dbl>  <int> <chr>     <chr>    <fct>  
#> 1  7.61  -0.215  | setosa      0.333     50 setosa    active   NA     
#> 2 -1.83   0.728  | versicolor  0.333     50 versicol… active   NA     
#> 3 -5.78  -0.513  | virginica   0.333     50 virginica active   NA     
#> 4  8.06  -0.300  | NA         NA         NA setosa    score    setosa 
#> 5  7.13   0.787  | NA         NA         NA setosa    score    setosa 
#>        ⋮                                    ⋮                         
#> # Columns (standard, 0%): [ 4 × 2 | 1 ]
#>      LD1     LD2 | vec.length
#>      [1]     [1] | <dbl>
#> 1  0.829 -0.0241 | 0.830
#> 2  1.53  -2.16   | 2.65 
#> 3 -2.20   0.932  | 2.39 
#> 4 -2.81  -2.84   | 3.99 

# bind data frames of annotations
iris_medians <-
  stats::aggregate(iris[, 1:4], median, by = iris[, 5, drop = FALSE])
# TODO: Requirement of `.elements` for matching is fragile.
iris_lda %>%
  # retain '.element' in order to match by `elements`
  select_rows(.element) %>%
  cbind_rows(iris_medians, elements = "active")
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | .element Species    Sepal.Length Sepal.Width
#>   [2366] [20.98] | <chr>    <fct>             <dbl>       <dbl>
#> 1  7.61  -0.215  | active   setosa              5           3.4
#> 2 -1.83   0.728  | active   versicolor          5.9         2.8
#> 3 -5.78  -0.513  | active   virginica           6.5         3  
#> 4  8.06  -0.300  | score    NA                 NA          NA  
#> 5  7.13   0.787  | score    NA                 NA          NA  
#>        ⋮                                ⋮                      
#> # ℹ 2 more variables: Petal.Length <dbl>,
#> #   Petal.Width <dbl>
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name         .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  
iris_lda %>%
  select_rows(name, Species) %>%
  left_join_rows(iris_medians, by = c("name" = "Species"))
#> # A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´
#> # 2 coordinates: LD1 and LD2
#> # Rows (principal, 100%): [ 153 × 2 | 6 ]
#>      LD1     LD2 | name  Species Sepal.Length Sepal.Width Petal.Length
#>   [2366] [20.98] | <chr> <fct>          <dbl>       <dbl>        <dbl>
#> 1  7.61  -0.215  | seto… NA               5           3.4         1.5 
#> 2 -1.83   0.728  | vers… NA               5.9         2.8         4.35
#> 3 -5.78  -0.513  | virg… NA               6.5         3           5.55
#> 4  8.06  -0.300  | NA    setosa          NA          NA          NA   
#> 5  7.13   0.787  | NA    setosa          NA          NA          NA   
#>        ⋮                                    ⋮                         
#> # ℹ 1 more variable: Petal.Width <dbl>
#> # Columns (standard, 0%): [ 4 × 2 | 2 ]
#>      LD1     LD2 | name         .element
#>      [1]     [1] | <chr>        <chr>   
#> 1  0.829 -0.0241 | Sepal.Length active  
#> 2  1.53  -2.16   | Sepal.Width  active  
#> 3 -2.20   0.932  | Petal.Length active  
#> 4 -2.81  -2.84   | Petal.Width  active  
```
