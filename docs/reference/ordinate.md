# Fit an ordination model to a data object

This is a convenience function to fit an ordination model to a data
object, wrap the result as a tbl_ord, and annotate this output with
metadata from the model and possibly from the data.

## Usage

``` r
ordinate(x, model, ...)

# Default S3 method
ordinate(x, model, ...)

# S3 method for class 'array'
ordinate(x, model, ...)

# S3 method for class 'table'
ordinate(x, model, ...)

# S3 method for class 'data.frame'
ordinate(x, model, cols, augment, ...)

# S3 method for class 'dist'
ordinate(x, model, ...)
```

## Arguments

- x:

  A data object to be passed to the `model`, such as an
  [array](https://rdrr.io/r/base/array.html),
  [table](https://rdrr.io/r/base/table.html),
  [data.frame](https://rdrr.io/r/base/data.frame.html), or
  [stats::dist](https://rdrr.io/r/stats/dist.html).

- model:

  An ordination function whose output is coercible to class
  '[tbl_ord](tbl_ord.md)', or a symbol or character string (handled by
  [`match.fun()`](https://rdrr.io/r/base/match.fun.html)).
  Alternatively, a formula `~ fun(., ...)` where `fun` is such a
  function and other arguments are explicit, which will be evaluated
  with `x` in place of `.`.

- ...:

  Additional arguments passed to `model`.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  If `x` is a data frame, columns to pass to `model`. If missing, all
  columns are used.

- augment:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  If `x` is a data frame, columns to augment to the row data of the
  ordination. If missing, all columns not included in `cols` will be
  augmented.

## Value

An augmented tbl_ord.

## Details

The default method fits the specified model to the provided data object,
wraps the result as a [tbl_ord](tbl_ord.md), and augments this output
with any intrinsic metadata from the model via
[`augment_ord()`](augmentation.md).

The default method is used for most classes, though this may change in
future. The [data.frame](https://rdrr.io/r/base/data.frame.html) method
allows the user to specify what columns to include in the model and what
columns with which to annotate the output.

## Examples

``` r
# LRA of arrest data
ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)
#> # A tbl_ord of class 'lra': (50 × 2) · (3 × 2)´
#> # 2 coordinates: LRSV1 and LRSV2
#> # Rows (standard, 0%): [ 50 × 2 | 4 ]
#>    LRSV1  LRSV2 | name       weight .element UrbanPop
#>      [1]    [1] | <chr>       <dbl> <chr>       <int>
#> 1 -0.680  0.930 | Alabama    0.0271 active         58
#> 2  0.930 -0.625 | Alaska     0.0318 active         48
#> 3 -0.330 -1.31  | Arizona    0.0333 active         80
#> 4 -0.351  0.277 | Arkansas   0.0219 active         50
#> 5  0.552 -1.00  | California 0.0326 active         91
#>        ⋮                           ⋮                 
#> # Columns (standard, 0%): [ 3 × 2 | 3 ]
#>    LRSV1  LRSV2 | name    weight .element
#>      [1]    [1] | <chr>    <dbl> <chr>   
#> 1  0.283  4.96  | Murder  0.0390 active  
#> 2  2.88  -0.366 | Rape    0.106  active  
#> 3 -0.371 -0.181 | Assault 0.855  active  

# CMDS of inter-city distance data
ordinate(UScitiesD, cmdscale_ord, k = 3L)
#> # A tbl_ord of class 'cmds_ord': (10 × 3) · (0 × 3)´
#> # 3 coordinates: PCo1, PCo2, PCo3
#> # Rows (principal, 100%): [ 10 × 3 | 3 ]
#>      PCo1   PCo2   PCo3 | name          .element Labels       
#> [9.2e+13][3e+12][7e+07] | <chr>         <chr>    <chr>        
#>  1  -719.  143.   35.1  | Atlanta       active   Atlanta      
#>  2  -382. -341.   29.6  | Chicago       active   Chicago      
#>  3   482.  -25.3  53.4  | Denver        active   Denver       
#>  4  -161.  573.    1.45 | Houston       active   Houston      
#>  5  1204.  390.  -18.6  | LosAngeles    active   LosAngeles   
#>  6 -1134.  582.  -32.3  | Miami         active   Miami        
#>  7 -1072. -519.  -34.3  | NewYork       active   NewYork      
#>  8  1421.  113.   -7.75 | SanFrancisco  active   SanFrancisco 
#>  9  1342. -580.  -23.7  | Seattle       active   Seattle      
#> 10  -980. -335.   -2.90 | Washington.DC active   Washington.DC
#> # Columns (standard, 0%): [ 0 × 3 | 1 ]
#>      PCo1   PCo2   PCo3 | 
#>     <dbl>  <dbl>  <dbl> | 

# PCA of iris data
ordinate(iris, princomp, cols = -Species, augment = c(Sepal.Width, Species))
#> # A tbl_ord of class 'princomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: Comp.1, Comp.2, ..., Comp.4
#> # Rows (principal, 100%): [ 150 × 4 | 3 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | .element Sepal.Width
#>     [630] [36.16] [11.65]  [3.551] | <chr>          <dbl>
#> 1 -2.68    0.319   0.0279  0.00226 | score            3.5
#> 2 -2.71   -0.177   0.210   0.0990  | score            3  
#> 3 -2.89   -0.145  -0.0179  0.0200  | score            3.2
#> 4 -2.75   -0.318  -0.0316 -0.0756  | score            3.1
#> 5 -2.73    0.327  -0.0901 -0.0613  | score            3.6
#>                 ⋮                             ⋮          
#> # ℹ 1 more variable:
#> #   Species <fct>
#> # Columns (standard, 0%): [ 4 × 4 | 4 ]
#>    Comp.1  Comp.2  Comp.3   Comp.4 | name       center scale
#>       [1]     [1]     [1]      [1] | <chr>       <dbl> <dbl>
#> 1  0.361   0.657   0.582   0.315   | Sepal.Len…   5.84     1
#> 2 -0.0845  0.730  -0.598  -0.320   | Sepal.Wid…   3.06     1
#> 3  0.857  -0.173  -0.0762 -0.480   | Petal.Len…   3.76     1
#> 4  0.358  -0.0755 -0.546   0.754   | Petal.Wid…   1.20     1
#> # ℹ 1 more variable:
#> #   .element <chr>
ordinate(iris, cols = 1:4, ~ prcomp(., center = TRUE, scale. = TRUE))
#> # A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´
#> # 4 coordinates: PC1, PC2, ..., PC4
#> # Rows (principal, 100%): [ 150 × 4 | 2 ]
#>      PC1     PC2     PC3     PC4 | .element Species
#>  [434.9] [136.2] [21.87] [3.087] | <chr>    <fct>  
#> 1 -2.26  -0.478   0.127   0.0241 | active   setosa 
#> 2 -2.07   0.672   0.234   0.103  | active   setosa 
#> 3 -2.36   0.341  -0.0441  0.0283 | active   setosa 
#> 4 -2.29   0.595  -0.0910 -0.0657 | active   setosa 
#> 5 -2.38  -0.645  -0.0157 -0.0358 | active   setosa 
#>                ⋮                          ⋮        
#> # Columns (standard, 0%): [ 4 × 4 | 4 ]
#>      PC1     PC2     PC3     PC4 | name       center scale
#>      [1]     [1]     [1]     [1] | <chr>       <dbl> <dbl>
#> 1  0.521 -0.377   0.720   0.261  | Sepal.Len…   5.84 0.828
#> 2 -0.269 -0.923  -0.244  -0.124  | Sepal.Wid…   3.06 0.436
#> 3  0.580 -0.0245 -0.142  -0.801  | Petal.Len…   3.76 1.77 
#> 4  0.565 -0.0669 -0.634   0.524  | Petal.Wid…   1.20 0.762
#> # ℹ 1 more variable:
#> #   .element <chr>

# CA of hair & eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
ordinate(haireye, MASS::corresp, cols = everything())
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

# FA of Swiss social data
ordinate(swiss, model = factanal, factors = 2L, scores = "Bartlett")
#> # A tbl_ord of class 'factanal': (47 × 2) · (6 × 2)´
#> # 2 coordinates: Factor1 and Factor2
#> # Rows (standard, 0%): [ 47 × 2 | 2 ]
#>    Factor1 Factor2 | .element name        
#>        [1]     [1] | <chr>    <chr>       
#>  1  0.0775 -0.673  | score    Courtelary  
#>  2 -0.177   1.14   | score    Delemont    
#>  3 -0.587   1.27   | score    Franches-Mnt
#>  4 -0.427  -0.169  | score    Moutier     
#>  5  0.382  -0.708  | score    Neuveville  
#>         ⋮                      ⋮          
#> # Columns (principal, 100%): [ 6 × 2 | 3 ]
#>    Factor1 Factor2 | name             uniqueness .element
#>    [2.311] [1.481] | <chr>                 <dbl> <chr>   
#>  1 -0.652   0.393  | Fertility            0.420  active  
#>  2 -0.631   0.333  | Agriculture          0.492  active  
#>  3  0.685  -0.510  | Examination          0.270  active  
#>  4  0.997  -0.0313 | Education            0.005  active  
#>  5 -0.124   0.961  | Catholic             0.0607 active  
#>  6 -0.0947  0.175  | Infant.Mortality     0.960  active  

# LDA of iris data
ordinate(iris, ~ lda_ord(.[, 1:4], .[, 5]))
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

# CCA of savings data
ordinate(
  LifeCycleSavings[, c("pop15", "pop75")],
  # second data set must be handled as an additional parameter to `model`
  y = LifeCycleSavings[, c("sr", "dpi", "ddpi")],
  model = cancor_ord, scores = TRUE
)
#> # A tbl_ord of class 'cancor_ord': (54 × 2) · (56 × 2)´
#> # 2 coordinates: CanCor1 and CanCor2
#> # Rows (standard, 0%): [ 54 × 2 | 3 ]
#>      CanCor1    CanCor2 | name      center .element
#>          [1]        [1] | <chr>      <dbl> <chr>   
#>  1 -0.00911  -0.0362    | pop15      35.1  active  
#>  2  0.0486   -0.260     | pop75       2.29 active  
#>  3  0.0804    0.0577    | Australia  NA    score   
#>  4  0.210    -0.125     | Austria    NA    score   
#>  5  0.207    -0.147     | Belgium    NA    score   
#>            ⋮                          ⋮            
#> # Columns (standard, 0%): [ 56 × 2 | 3 ]
#>      CanCor1    CanCor2 | name       center .element
#>          [1]        [1] | <chr>       <dbl> <chr>   
#>  1  0.00847   0.0334    | sr           9.67 active  
#>  2  0.000131 -0.0000759 | dpi       1107.   active  
#>  3  0.00417  -0.0123    | ddpi         3.76 active  
#>  4  0.171    -0.0232    | Australia   NA    score   
#>  5  0.0735    0.0475    | Austria     NA    score   
#>            ⋮                          ⋮             
```
