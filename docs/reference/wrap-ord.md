# Wrappers for lossy ordination methods

These `*_ord` functions wrap core R functions with modifications for use
with '[tbl_ord](tbl_ord.md)' methods. Some parameters are hidden from
the user and set to settings required for these methods, some matrix
outputs are given row or column names to be used by them, and new
'\*\_ord' S3 class attributes are added to enable them.

## Usage

``` r
eigen_ord(x, symmetric = isSymmetric.matrix(x))

svd_ord(x, nu = min(dim(x)), nv = min(dim(x)))

cmdscale_ord(d, k = 2, add = FALSE)

cancor_ord(x, y, xcenter = TRUE, ycenter = TRUE, scores = FALSE)
```

## Arguments

- x:

  a numeric or complex matrix whose spectral decomposition is to be
  computed. Logical matrices are coerced to numeric.

- symmetric:

  if `TRUE`, the matrix is assumed to be symmetric (or Hermitian if
  complex) and only its lower triangle (diagonal included) is used. If
  `symmetric` is not specified,
  [`isSymmetric`](https://rdrr.io/r/base/isSymmetric.html)`(x)` is used.

- nu:

  the number of left singular vectors to be computed. This must between
  `0` and `n = nrow(x)`.

- nv:

  the number of right singular vectors to be computed. This must be
  between `0` and `p = ncol(x)`.

- d:

  a distance structure such as that returned by `dist` or a full
  symmetric matrix containing the dissimilarities.

- k:

  the maximum dimension of the space which the data are to be
  represented in; must be in \\\\1, 2, \ldots, n-1\\\\.

- add:

  logical indicating if an additive constant \\c\*\\ should be computed,
  and added to the non-diagonal dissimilarities such that the modified
  dissimilarities are Euclidean.

- y:

  numeric matrix (\\n \times p_2\\), containing the y coordinates.

- xcenter:

  logical or numeric vector of length \\p_1\\, describing any centering
  to be done on the x values before the analysis. If `TRUE` (default),
  subtract the column means. If `FALSE`, do not adjust the columns.
  Otherwise, a vector of values to be subtracted from the columns.

- ycenter:

  analogous to `xcenter`, but for the y values.

- scores:

  Logical; whether to return canonical scores and structure
  correlations.

## Value

Objects slightly modified from the outputs of the original functions,
with new '\*-ord' classes.

## Details

The following table summarizes the wrapped functions:

|  |  |  |  |  |
|----|----|----|----|----|
| Original function | Hide params | New params | Add names | New class |
| [`base::eigen()`](https://rdrr.io/r/base/eigen.html) | Yes | No | Yes | Yes |
| [`base::svd()`](https://rdrr.io/r/base/svd.html) | Yes | No | Yes | Yes |
| [`stats::cmdscale()`](https://rdrr.io/r/stats/cmdscale.html) | Yes | No | No | Yes |
| [`stats::cancor()`](https://rdrr.io/r/stats/cancor.html) | No | Yes | No | Yes |

**NB:** `eigen_ord()` is deprecated in favor of
[`eigen()`](https://rdrr.io/r/base/eigen.html), which for some time has
returned an S3 class (with recovery methods in **ordr**).

By default, `cancor_ord()` returns the same data as
[`stats::cancor()`](https://rdrr.io/r/stats/cancor.html): the canonical
correlations (`cor`), the canonical coefficients (`$xcoef` and
`$ycoef`), and the variable means (`$xcenter`, `$ycenter`). If
`scores = TRUE`, then `cancor_ord()` also returns the scores `$xscores`
and `$yscores` calculated from the (appropriately centered) data and the
coefficients and the intraset structure correlations `$xstructure` and
`$ystructure` between these and the data. These modifications are
inspired by the [`cancor()`](https://rdrr.io/r/stats/cancor.html)
function in **candisc**, though two caveats should be noted: First, the
canonical coefficients (hence the canonical scores) are scaled by \\n -
1\\ compared to these, though the intraset structure correlations are
the same. Second, the *interset* structure correlations are not
returned, as these may be obtained by conferring inertia unto the
intraset ones.

## Examples

``` r
# glass composition data from one furnace
glass_banias <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
# eigendecomposition of a covariance matrix
(glass_cov <- cov(glass_banias))
#>             SiO2       Na2O        CaO    Al2O3         MgO      K2O
#> SiO2   2.6155367  0.6093667 -1.2869867 -0.45252 -0.46817333 -0.31149
#> Na2O   0.6093667  0.2559067 -0.3217067 -0.16542 -0.10587333 -0.10726
#> CaO   -1.2869867 -0.3217067  0.7060267  0.24842  0.20559333  0.13220
#> Al2O3 -0.4525200 -0.1654200  0.2484200  0.12716  0.07972000  0.07230
#> MgO   -0.4681733 -0.1058733  0.2055933  0.07972  0.09462667  0.06682
#> K2O   -0.3114900 -0.1072600  0.1322000  0.07230  0.06682000  0.06131
eigen_ord(glass_cov)
#> eigen() decomposition
#> $values
#> [1] 3.617285e+00 1.467376e-01 8.282650e-02 1.360291e-02 1.141672e-04
#> [6] 1.958398e-17
#> 
#> $vectors
#>              EV1         EV2         EV3         EV4        EV5        EV6
#> SiO2   0.8469970  0.31716695 -0.25837288 -0.11577441 -0.2822267  0.1489597
#> Na2O   0.2099520 -0.79170694 -0.10021025 -0.51787194  0.1385212  0.1780413
#> CaO   -0.4260986  0.04199369 -0.76882445 -0.05663831 -0.3233201  0.3432808
#> Al2O3 -0.1556273  0.45906193 -0.08066526 -0.77062798  0.2878426 -0.2860325
#> MgO   -0.1492104 -0.07286372  0.38010592 -0.30504020 -0.8407925 -0.1672273
#> K2O   -0.1023356  0.23408287  0.42558539 -0.16816668  0.0826960  0.8476385
#> 
# singular value decomposition of a data matrix
svd_ord(glass_banias)
#> $d
#> [1] 177.16843535   2.56375788   0.82468871   0.51400398   0.21898181
#> [6]   0.01400983
#> 
#> $u
#>          SV1          SV2         SV3        SV4        SV5         SV6
#> 1 -0.4114474  0.268953258  0.54464383 -0.4529219  0.4351102  0.25938230
#> 2 -0.4099492  0.124966122  0.51481370  0.3162816 -0.6000110 -0.30206743
#> 3 -0.4097327 -0.103523503 -0.13351247  0.5115444  0.6162163 -0.40271096
#> 4 -0.3914429 -0.815275824 -0.07487103 -0.3907913 -0.1362392 -0.07220201
#> 5 -0.4168814  0.486413365 -0.59131457 -0.3710452 -0.1857274 -0.26036475
#> 6 -0.4095779 -0.007607042 -0.25543255  0.3778329 -0.1337444  0.77865078
#> 
#> $v
#>               SV1         SV2         SV3          SV4         SV5         SV6
#> SiO2  -0.97757835  0.09992541 -0.16303444  0.046753453 -0.05969843 -0.04500518
#> Na2O  -0.16869103  0.14528933  0.81605152 -0.238508341  0.42108755  0.22426170
#> CaO   -0.11569079 -0.87296399  0.28883998  0.323881874 -0.16853953  0.08840776
#> Al2O3 -0.01665686 -0.29609746 -0.38102381  0.008014293  0.87323378 -0.06533172
#> MgO   -0.03775856 -0.28137368  0.06285458 -0.713218748 -0.10916969 -0.62837365
#> K2O   -0.02822033 -0.19998734 -0.27371758 -0.572086712 -0.12751682  0.73535272
#> 
#> attr(,"class")
#> [1] "svd_ord"
# classical multidimensional scaling of a distance matrix
cmdscale_ord(dist(glass_banias))
#> $points
#>             [,1]        [,2]
#> [1,] -0.87986198  0.53544035
#> [2,] -0.45244074  0.40907498
#> [3,] -0.08144239 -0.26484655
#> [4,]  3.61567304 -0.04227923
#> [5,] -1.99639680 -0.36657720
#> [6,] -0.20553113 -0.27081236
#> 
#> $eig
#> [1] 1.808643e+01 7.336881e-01 4.141325e-01 6.801456e-02 5.708361e-04
#> [6] 6.300478e-16
#> 
#> $x
#>              [,1]        [,2]        [,3]        [,4]       [,5]         [,6]
#> [1,] -2.207855556 -1.12728889  0.22354444   6.3455444 -3.2300556 -0.003888889
#> [2,] -1.127288889 -0.87692222  0.03611111   3.3851111 -1.3678889 -0.049122222
#> [3,]  0.223544444  0.03611111 -0.47385556   0.7213444 -0.2278556 -0.279288889
#> [4,]  6.345544444  3.38511111  0.72134444 -26.2322556 14.2529444  1.527311111
#> [5,] -3.230055556 -1.36788889 -0.22785556  14.2529444 -8.5234556 -0.903688889
#> [6,] -0.003888889 -0.04912222 -0.27928889   1.5273111 -0.9036889 -0.291322222
#> 
#> $ac
#> [1] 0
#> 
#> $GOF
#> [1] 0.9749924 0.9749924
#> 
#> attr(,"class")
#> [1] "cmds_ord"

# canonical correlation analysis with trace components
glass_banias_minor <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("TiO2", "FeO", "MnO", "P2O5", "Cl", "SO3")
)
# impute value just below smallest recorded value
glass_banias_minor$TiO2[[1L]] <- 0.5
cancor_ord(glass_banias, glass_banias_minor)
#> $cor
#> [1] 1 1 1 1 1
#> 
#> $xcoef
#>             [,1]       [,2]       [,3]       [,4]      [,5]
#> SiO2  -2.1673652  -7.306234  -8.578817  3.8513012 -2.879401
#> Na2O   0.2801947   3.744374   2.712699 -0.8050915  2.843217
#> CaO   -1.9324129  -8.473998 -10.315478  5.3830097 -3.775547
#> Al2O3 -0.3310140   9.387447   7.761524 -4.0097416  4.535237
#> MgO   -6.6314866 -21.879448 -22.638477 10.6364947 -6.686017
#> 
#> $ycoef
#>            [,1]        [,2]       [,3]       [,4]     [,5]
#> TiO2  -3.475099  -1.2491210  -1.657125 -0.3078673 2.960014
#> FeO    4.579395   2.5869489  -2.202136  5.5897345 0.000000
#> MnO    6.052069   0.1289956   2.415295  2.9325505 0.000000
#> P2O5 -12.231302 -15.4358577 -12.041165 -0.8048454 0.000000
#> Cl    26.928841   2.7068806   2.381791 10.9489065 0.000000
#> 
#> $xcenter
#>      SiO2      Na2O       CaO     Al2O3       MgO       K2O 
#> 70.691667 12.196667  8.383333  1.210000  2.736667  2.045000 
#> 
#> $ycenter
#>      TiO2       FeO       MnO      P2O5        Cl       SO3 
#> 0.1933333 0.4300000 0.9666667 0.2700000 0.8100000 0.3483333 
#> 
#> attr(,"class")
#> [1] "cancor_ord"

# calculate canonical scores and structure correlations
glass_cca <-
  cancor_ord(glass_banias[, 1:3], glass_banias_minor[, 1:3], scores = TRUE)
# scores
glass_cca$xscores
#>             [,1]         [,2]       [,3]
#> [1,]  0.08625757  0.564009626  0.5086737
#> [2,] -0.06325679  0.009686013  0.4449230
#> [3,] -0.31243762 -0.589123478  0.1458326
#> [4,]  0.85114073 -0.156299524 -0.2733283
#> [5,] -0.34697599  0.470284132 -0.6433738
#> [6,] -0.21472789 -0.298556769 -0.1827271
# intraset correlations
glass_cca$xstructure
#>            [,1]       [,2]       [,3]
#> SiO2 -0.9261925  0.3749912 0.03935800
#> Na2O -0.5467957  0.5715073 0.61187732
#> CaO   0.7639885 -0.6435255 0.04686735
# interset correlations
glass_cca$xstructure %*% diag(glass_cca$cor)
#>            [,1]       [,2]       [,3]
#> SiO2 -0.9261925  0.3594308 0.02748688
#> Na2O -0.5467957  0.5477923 0.42732349
#> CaO   0.7639885 -0.6168220 0.03273126
```
