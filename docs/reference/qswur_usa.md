# U.S. university rankings

Classifications and rankings of U.S. universities for the years
2017–2020.

## Usage

``` r
data(qswur_usa)
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) of 13
variables on 612 cases:

- `year`:

  year of rankings

- `institution`:

  institution of higher learning

- `size`:

  size category of institution

- `focus`:

  subject range of institution

- `res`:

  research intensity of institution

- `age`:

  age classification of institution

- `status`:

  status of institution

- `rk_academic`:

  rank by academic reputation

- `rk_employer`:

  rank by employer reputation

- `rk_ratio`:

  rank by faculty–student ratio

- `rk_citations`:

  rank by citations per faculty

- `rk_intl_faculty`:

  rank by international faculty ratio

- `rk_intl_students`:

  rank by international student ratio

## Source

Quacquarelli Symonds (2021).

## Details

Ranking data were obtained from the public QS website.

## References

Quacquarelli Symonds (2021) "University Rankings". TopUniversities.com
<https://www.topuniversities.com/university-rankings>.

## Examples

``` r
# subset QS data to rank variables
head(qswur_usa)
#> # A tibble: 6 × 13
#>    year institution       size  focus res     age status rk_academic rk_employer
#>   <int> <chr>             <fct> <fct> <fct> <int> <chr>        <int>       <int>
#> 1  2017 MASSACHUSETTS IN… M     CO    VH        5 B                6           4
#> 2  2017 STANFORD UNIVERS… L     FC    VH        5 A                5           5
#> 3  2017 HARVARD UNIVERSI… L     FC    VH        5 B                1           1
#> 4  2017 CALIFORNIA INSTI… S     CO    VH        5 B               23          90
#> 5  2017 UNIVERSITY OF CH… L     FC    VH        5 B               13          47
#> 6  2017 PRINCETON UNIVER… M     CO    VH        5 B               10          32
#> # ℹ 4 more variables: rk_ratio <int>, rk_citations <int>,
#> #   rk_intl_faculty <int>, rk_intl_students <int>
qs_ranks <- subset(
  qswur_usa,
  complete.cases(qswur_usa),
  select = 8:13
)
# calculate Kendall correlation matrix
qs_cor <- cor(qs_ranks, method = "kendall")

# calculate eigendecomposition
qs_eigen <- eigen_ord(qs_cor)
# view correlations as cosines of biplot vectors
biplot(x = qs_eigen$vectors, y = qs_eigen$vectors, col = c(NA, "black"))
```
