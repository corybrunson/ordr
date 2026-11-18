# Functionality for singular value decompositions

These methods extract data from, and attribute new data to, objects of
class `"svd_ord"` returned by [`svd_ord()`](wrap-ord.md).

## Usage

``` r
# S3 method for class 'svd_ord'
as_tbl_ord(x)

# S3 method for class 'svd_ord'
recover_rows(x)

# S3 method for class 'svd_ord'
recover_cols(x)

# S3 method for class 'svd_ord'
recover_inertia(x)

# S3 method for class 'svd_ord'
recover_coord(x)

# S3 method for class 'svd_ord'
recover_conference(x)

# S3 method for class 'svd_ord'
recover_aug_rows(x)

# S3 method for class 'svd_ord'
recover_aug_cols(x)

# S3 method for class 'svd_ord'
recover_aug_coord(x)
```

## Arguments

- x:

  An ordination object.

## Value

The recovery generics `recover_*()` return [core model
components](recoverers.md), [distribution of inertia](conference.md),
[supplementary elements](supplementation.md), and [intrinsic
metadata](augmentation.md); but they require methods for each model
class to tell them what these components are.

The generic [`as_tbl_ord()`](tbl_ord.md) returns its input wrapped in
the 'tbl_ord' class. Its methods determine what model classes it is
allowed to wrap. It then provides 'tbl_ord' methods with access to the
recoverers and hence to the model components.

## See also

Other methods for singular value decomposition-based techniques:
[`methods-cancor`](methods-cancor.md),
[`methods-correspondence`](methods-correspondence.md),
[`methods-lda`](methods-lda.md), [`methods-lra`](methods-lra.md),
[`methods-mca`](methods-mca.md), [`methods-prcomp`](methods-prcomp.md)

Other models from the base package: [`methods-eigen`](methods-eigen.md)

## Examples

``` r
# matrix of U.S. personal expenditure data
class(USPersonalExpenditure)
#> [1] "matrix" "array" 
print(USPersonalExpenditure)
#>                       1940   1945  1950 1955  1960
#> Food and Tobacco    22.200 44.500 59.60 73.2 86.80
#> Household Operation 10.500 15.500 29.00 36.5 46.20
#> Medical and Health   3.530  5.760  9.71 14.0 21.10
#> Personal Care        1.040  1.980  2.45  3.4  5.40
#> Private Education    0.341  0.974  1.80  2.6  3.64
# singular value decomposition into row and column coordinates
USPersonalExpenditure %>%
  svd_ord() %>%
  as_tbl_ord() %>%
  print() -> spend_svd
#> # A tbl_ord of class 'svd_ord': (5 × 5) · (5 × 5)´
#> # 5 coordinates: SV1, SV2, ..., SV5
#> # Rows (standard, 0%): [ 5 × 5 | 0 ]
#>        SV1     SV2     SV3     SV4     SV5 | 
#>        [1]     [1]     [1]     [1]     [1] | 
#>  1 -0.881   0.456  -0.122   0.0245 -0.0299 | 
#>  2 -0.436  -0.677   0.583  -0.0763  0.0778 | 
#>  3 -0.176  -0.562  -0.734   0.0411 -0.336  | 
#>  4 -0.0455 -0.0929 -0.314  -0.494   0.805  | 
#>  5 -0.0311 -0.0989 -0.0892  0.865   0.483  | 
#> # Columns (standard, 0%): [ 5 × 5 | 0 ]
#>        SV1     SV2     SV3     SV4     SV5 | 
#>        [1]     [1]     [1]     [1]     [1] | 
#>  1 -0.159   0.113   0.182  -0.897  -0.351  | 
#>  2 -0.302   0.792  -0.527   0.0265  0.0499 | 
#>  3 -0.429   0.211   0.620  -0.0146  0.622  | 
#>  4 -0.532   0.0266  0.302   0.421  -0.669  | 
#>  5 -0.645  -0.561  -0.461  -0.129   0.201  | 

# recover matrices of row and column coordinates
get_rows(spend_svd)
#>                             SV1         SV2         SV3         SV4         SV5
#> Food and Tobacco    -0.88086766  0.45563080 -0.12237718  0.02450591 -0.02993361
#> Household Operation -0.43580483 -0.67662102  0.58341818 -0.07630369  0.07784028
#> Medical and Health  -0.17637018 -0.56230756 -0.73372346  0.04111351 -0.33565360
#> Personal Care       -0.04553400 -0.09285599 -0.31360568 -0.49361448  0.80454996
#> Private Education   -0.03108899 -0.09885616 -0.08915387  0.86500383  0.48278452
get_cols(spend_svd)
#>             SV1         SV2        SV3         SV4         SV5
#> 1940 -0.1589586  0.11313761  0.1824780 -0.89728506 -0.35144462
#> 1945 -0.3016855  0.79223017 -0.5274149  0.02654943  0.04985844
#> 1950 -0.4293572  0.21081041  0.6202698 -0.01464040  0.62150007
#> 1955 -0.5323309  0.02659741  0.3024320  0.42107353 -0.66869157
#> 1960 -0.6449761 -0.56073415 -0.4607988 -0.12906349  0.20146975

# augment with row and column names
augment_ord(spend_svd)
#> # A tbl_ord of class 'svd_ord': (5 × 5) · (5 × 5)´
#> # 5 coordinates: SV1, SV2, ..., SV5
#> # Rows (standard, 0%): [ 5 × 5 | 2 ]
#>        SV1     SV2     SV3     SV4     SV5 | name           .element
#>        [1]     [1]     [1]     [1]     [1] | <chr>          <chr>   
#>  1 -0.881   0.456  -0.122   0.0245 -0.0299 | Food and Toba… active  
#>  2 -0.436  -0.677   0.583  -0.0763  0.0778 | Household Ope… active  
#>  3 -0.176  -0.562  -0.734   0.0411 -0.336  | Medical and H… active  
#>  4 -0.0455 -0.0929 -0.314  -0.494   0.805  | Personal Care  active  
#>  5 -0.0311 -0.0989 -0.0892  0.865   0.483  | Private Educa… active  
#> # Columns (standard, 0%): [ 5 × 5 | 2 ]
#>        SV1     SV2     SV3     SV4     SV5 | name  .element
#>        [1]     [1]     [1]     [1]     [1] | <chr> <chr>   
#>  1 -0.159   0.113   0.182  -0.897  -0.351  | 1940  active  
#>  2 -0.302   0.792  -0.527   0.0265  0.0499 | 1945  active  
#>  3 -0.429   0.211   0.620  -0.0146  0.622  | 1950  active  
#>  4 -0.532   0.0266  0.302   0.421  -0.669  | 1955  active  
#>  5 -0.645  -0.561  -0.461  -0.129   0.201  | 1960  active  
# initial matrix decomposition confers no inertia to coordinates
get_conference(spend_svd)
#> [1] 0 0
```
