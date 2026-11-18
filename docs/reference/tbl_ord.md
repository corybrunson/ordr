# A unified ordination object class

These functions wrap ordination objects in the class tbl_ord, create
tbl_ords directly from matrices, and test for the class and basic
structure.

## Usage

``` r
as_tbl_ord(x)

# S3 method for class 'tbl_ord'
as_tbl_ord(x)

make_tbl_ord(rows = NULL, cols = NULL, ...)

is_tbl_ord(x)

is.tbl_ord(x)

valid_tbl_ord(x)

un_tbl_ord(x)
```

## Arguments

- x:

  An ordination object.

- rows, cols:

  Matrices to be used as factors of a tbl_ord.

- ...:

  Additional elements of a custom tbl_ord.

## Value

A tbl_ord (`as*()`, `make*()`), an S3-class model object that can be
wrapped as one (`un*()`), or a logical value (`is*()`, `value*()`).

## Details

The tbl_ord class wraps around a range of ordination classes, making
available a suite of ordination tools that specialize to each original
object class. These tools include [`format()`](format.md) and
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
methods, which facilitate the
[`print()`](https://rdrr.io/r/base/print.html) method and the
[`ggbiplot()`](ggbiplot.md) function.

No default method is provided for `as_tbl_ord()`, despite most defined
methods being equivalent (simply appending 'tbl_ord' to the vector of
object classes). This prevents objects for which other methods are not
defined from being re-classed as tbl_ords.

The function `make_tbl_ord()` creates a tbl_ord structured as a list of
two matrices, `u` and `v`, which must have the same number of columns
and the same column names.

`is_tbl_ord()` checks an object `x` for the tbl_ord class;
`valid_tbl_ord()` additionally checks for consistency between
`recover_coord(x)` and the columns of `recover_rows(x)` and
`recover_cols(x)`, using the [recoverers](recoverers.md). `un_tbl_ord()`
removes attributes associated with the tbl_ord class in order to restore
an object that was originally passed to `as_tbl_ord`.

## Examples

``` r
# illustrative ordination: FA of Swiss social data
swiss_fa <- factanal(swiss, factors = 3L, scores = "regression")
print(swiss_fa)
#> 
#> Call:
#> factanal(x = swiss, factors = 3L, scores = "regression")
#> 
#> Uniquenesses:
#>        Fertility      Agriculture      Examination        Education 
#>            0.005            0.286            0.213            0.114 
#>         Catholic Infant.Mortality 
#>            0.083            0.743 
#> 
#> Loadings:
#>                  Factor1 Factor2 Factor3
#> Fertility        -0.512   0.203   0.832 
#> Agriculture      -0.774   0.312  -0.129 
#> Examination       0.751  -0.423  -0.211 
#> Education         0.901          -0.262 
#> Catholic         -0.186   0.913   0.220 
#> Infant.Mortality                  0.500 
#> 
#>                Factor1 Factor2 Factor3
#> SS loadings      2.273   1.164   1.120
#> Proportion Var   0.379   0.194   0.187
#> Cumulative Var   0.379   0.573   0.759
#> 
#> The degrees of freedom for the model is 0 and the fit was 1e-04 

# add the 'tbl_ord' wrapper
swiss_fa_ord <- as_tbl_ord(swiss_fa)
# inspect wrapped model
is_tbl_ord(swiss_fa_ord)
#> [1] TRUE
print(swiss_fa_ord)
#> # A tbl_ord of class 'factanal': (47 × 3) · (6 × 3)´
#> # 3 coordinates: Factor1, Factor2, Factor3
#> # Rows (standard, 0%): [ 47 × 3 | 0 ]
#>    Factor1 Factor2 Factor3 | 
#>        [1]     [1]     [1] | 
#>  1  0.453  -0.900    1.46  | 
#>  2 -0.143   0.886    0.942 | 
#>  3 -0.308   0.848    1.75  | 
#>  4 -0.125  -0.463    1.53  | 
#>  5  0.341  -0.846    1.05  | 
#>             ⋮                
#> # Columns (principal, 100%): [ 6 × 3 | 0 ]
#>    Factor1 Factor2 Factor3 | 
#>    [2.273] [1.164]  [1.12] | 
#>  1 -0.512   0.203    0.832 | 
#>  2 -0.774   0.312   -0.129 | 
#>  3  0.751  -0.423   -0.211 | 
#>  4  0.901   0.0784  -0.262 | 
#>  5 -0.186   0.913    0.220 | 
#>  6  0.0289  0.0772   0.500 | 
valid_tbl_ord(swiss_fa_ord)
#> [1] TRUE
# unwrap the model
un_tbl_ord(swiss_fa_ord)
#> 
#> Call:
#> factanal(x = swiss, factors = 3L, scores = "regression")
#> 
#> Uniquenesses:
#>        Fertility      Agriculture      Examination        Education 
#>            0.005            0.286            0.213            0.114 
#>         Catholic Infant.Mortality 
#>            0.083            0.743 
#> 
#> Loadings:
#>                  Factor1 Factor2 Factor3
#> Fertility        -0.512   0.203   0.832 
#> Agriculture      -0.774   0.312  -0.129 
#> Examination       0.751  -0.423  -0.211 
#> Education         0.901          -0.262 
#> Catholic         -0.186   0.913   0.220 
#> Infant.Mortality                  0.500 
#> 
#>                Factor1 Factor2 Factor3
#> SS loadings      2.273   1.164   1.120
#> Proportion Var   0.379   0.194   0.187
#> Cumulative Var   0.379   0.573   0.759
#> 
#> The degrees of freedom for the model is 0 and the fit was 1e-04 

# create a 'tbl_ord' directly from row and column factors
# (missing inertia & other attributes)
swiss_fa_ord2 <- make_tbl_ord(rows = swiss_fa$scores, cols = swiss_fa$loadings)
# inspect wrapped factors
is_tbl_ord(swiss_fa_ord2)
#> [1] TRUE
print(swiss_fa_ord2)
#> # A tbl_ord: (47 × 3) · (6 × 3)´
#> # 3 coordinates: Factor1, Factor2, Factor3
#> # Rows: [ 47 × 3 | 0 ]
#>    Factor1 Factor2 Factor3 | 
#>      <dbl>   <dbl>   <dbl> | 
#>  1  0.453  -0.900    1.46  | 
#>  2 -0.143   0.886    0.942 | 
#>  3 -0.308   0.848    1.75  | 
#>  4 -0.125  -0.463    1.53  | 
#>  5  0.341  -0.846    1.05  | 
#>             ⋮                
#> # Columns: [ 6 × 3 | 0 ]
#>    Factor1 Factor2 Factor3 | 
#>      <dbl>   <dbl>   <dbl> | 
#>  1 -0.512   0.203    0.832 | 
#>  2 -0.774   0.312   -0.129 | 
#>  3  0.751  -0.423   -0.211 | 
#>  4  0.901   0.0784  -0.262 | 
#>  5 -0.186   0.913    0.220 | 
#>  6  0.0289  0.0772   0.500 | 
valid_tbl_ord(swiss_fa_ord2)
#> [1] TRUE
# unwrap factors
un_tbl_ord(swiss_fa_ord2)
#> $rows
#>                  Factor1    Factor2     Factor3
#> Courtelary    0.45318326 -0.8996943  1.45754866
#> Delemont     -0.14322063  0.8855428  0.94227871
#> Franches-Mnt -0.30797336  0.8477030  1.74514043
#> Moutier      -0.12534083 -0.4629021  1.52823000
#> Neuveville    0.34109514 -0.8458178  1.04687165
#> Porrentruy   -0.04473114  1.0029812  0.32499084
#> Broye        -0.20957031  0.9388742  0.95013743
#> Glane        -0.07584684  0.9555256  1.84540889
#> Gruyere      -0.17406590  1.0769276  0.80852222
#> Sarine        0.48706293  0.9876863  1.28186031
#> Veveyse      -0.21675238  1.0104124  1.24699185
#> Aigle        -0.10911693 -0.6583181 -0.49672411
#> Aubonne      -0.68437504 -0.8402563 -0.52979298
#> Avenches     -0.03638406 -0.7941117  0.04684235
#> Cossonay     -0.63417597 -0.9062500 -0.97644764
#> Echallens    -0.85262069 -0.5223686 -0.56695418
#> Grandson     -0.07196915 -0.9996237  0.34908713
#> Lausanne      1.61585928 -0.2740672 -0.32842180
#> La Vallee     1.11176971 -0.7484136 -0.66355682
#> Lavaux       -0.45759227 -0.8101884 -0.57253943
#> Morges       -0.16023764 -0.8189392 -0.35143678
#> Moudon       -0.76689425 -0.8871886 -0.73359873
#> Nyone         0.01148304 -0.4441114 -1.18020392
#> Orbe         -0.53346789 -0.8042945 -1.34826105
#> Oron         -1.11777160 -1.0181496 -0.21175864
#> Payerne      -0.36904017 -0.8817496  0.37541143
#> Paysd'enhaut -1.13077900 -0.8986374 -0.29968480
#> Rolle        -0.42160714 -0.6023891 -1.03877760
#> Vevey         0.94345222 -0.3409626 -0.46681372
#> Yverdon      -0.32587490 -0.7719262 -0.45767841
#> Conthey      -1.31619000  1.3250097 -0.61688868
#> Entremont    -0.89250420  1.4492843 -0.97170425
#> Herens       -1.23489054  1.2885602 -0.38378811
#> Martigwy     -0.65337979  1.3258772 -0.68175988
#> Monthey      -0.77161554  1.1439436  0.14336145
#> St Maurice   -0.58929096  1.5132175 -1.21362340
#> Sierre       -1.07854088  1.0949088  1.16699564
#> Sion          0.07266005  1.2536309  0.60709606
#> Boudry        0.42781724 -0.9511974  0.51240946
#> La Chauxdfnd  0.80305682 -0.8488623  0.28442301
#> Le Locle      0.63722154 -0.8395794  0.83668414
#> Neuchatel     2.31502030 -0.3320992  0.93658497
#> Val de Ruz   -0.19124613 -1.0328917  0.84358034
#> ValdeTravers  0.26693510 -0.9979172  0.17191080
#> V. De Geneve  3.72546960  1.0799755 -1.35159069
#> Rive Droite   1.02570325  0.9919321 -2.04348421
#> Rive Gauche   1.45927665  1.0609142 -1.96687793
#> 
#> $cols
#> 
#> Loadings:
#>                  Factor1 Factor2 Factor3
#> Fertility        -0.512   0.203   0.832 
#> Agriculture      -0.774   0.312  -0.129 
#> Examination       0.751  -0.423  -0.211 
#> Education         0.901          -0.262 
#> Catholic         -0.186   0.913   0.220 
#> Infant.Mortality                  0.500 
#> 
#>                Factor1 Factor2 Factor3
#> SS loadings      2.273   1.164   1.120
#> Proportion Var   0.379   0.194   0.187
#> Cumulative Var   0.379   0.573   0.759
#> 
```
