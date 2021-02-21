
<!-- edit README.rmd -->

[![Travis](https://travis-ci.org/corybrunson/ordr.svg?branch=main)](https://travis-ci.org/corybrunson/ordr)

# ordr

**ordr** is designed to integrate ordination analysis and biplot
visualization into a
[**tidyverse**](https://github.com/tidyverse/tidyverse) workflow.

## motivation

### ordination and biplots

*Ordination* is a catch-all term for a variety of statistical techniques
that introduce an artificial coordinate system for a data set in such a
way that a few coordinates capture a large amount of the data structure
[1]. The branch of mathematical statistics called [geometric data
analysis](https://www.springer.com/us/book/9781402022357) (GDA) provides
the theoretical basis for these techniques. Together with regression,
ordination can be contrasted to clustering and classification, in that
they assign continuous rather than discrete values to data elements.

Most ordination techniques decompose a numeric rectangular data set into
the product of two (or more) matrices. In some cases, such as principal
components analysis, the decomposition is exact; in others, such as
non-negative matrix factorization, it is approximate. Some techniques,
such as correspondence analysis, transform the data before
decomposition. Ordination techniques may be supervised, like linear
discriminant analysis, or unsupervised, like multidimensional scaling.
Analysis pipelines that use these techniques may use the artificial
coordinates directly, in place of natural coordinates, to arrange and
compare data elements or to predict responses. They also frequently use
*biplots* to represent the data elements that index the original table
on a shared set of axes.

The underlying matrix decomposition usually produces two matrices, say
*U* and *V*, corresponding to the rows and to the columns of the
original data matrix *X*: If *X* has *n* rows and *m* columns, then *U*
and *V* have *n* rows and *m* rows, respectively, and both have *r*
columns, with *r* being the rank of the ordination [2]. These *r*
columns provide a set of shared coordinates for the rows and columns of
*X*. If *X* contains a measured value for each of several variables
(columns) on each of several cases (rows), then the ordination provides
a space in which both cases and variables can be situated. If *X* is a
frequency table for two categorical variables, then the values of the
two variables can be overlaid on the same *r* shared coordinates. Such a
scatterplot is called a biplot.

### implementations in R

An extensive range of ordination techniques are already implemented in
R, from classical multidimensional scaling (`stats::cmdscale()`) and
principal components analysis (`stats::prcomp()` and
`stats::princomp()`) in the **stats** package distributed with base R,
across widely-used implementations of linear discriminant analysis
(`MASS::lda()`) and correspondence analysis (`ca::ca()`) in general-use
statistical packages, to highly specialized packages that implement
cutting-edge techniques or adapt conventional techniques to challenging
settings. These implementations come with their own conventions,
tailored to the research communities that produced them, and it would be
impractical (and probably unhelpful) to try to consolidate them.

Instead, **ordr** provides a streamlined process by which the outputs of
these methods—in particular, the matrix factors into which the original
data are approximately decomposed and the artificial coordinates they
share—can be inspected, annotated, tidied, and visualized. On this last
point, most biplot implementations in R provide limited customizability,
and **ordr** adopts the grammar of graphics paradigm from **ggplot2** to
modularize biplot elements. The package as a whole is designed to follow
the broader syntactic and grammatical conventions of the **tidyverse**,
so that users familiar with a this workflow can more easily and quickly
integrate ordination models into practice.

## usage

### installation

**ordr** remains under development and is not scheduled for a CRAN
release. For now, it can be installed from the (default) `main` branch
using [**remotes**](https://github.com/r-lib/remotes):

``` r
remotes::install_github("corybrunson/ordr")
```

### PCA example

Principal components analysis (PCA) is by far the most widely-used
ordination technique. This example performs an uncentered PCA on a small
data set of personal expenditures over twenty years and represents the
data in a symmetric biplot. At each step the result is printed, so that
the user can see the effect of each step on the `prcomp` object.

``` r
USPersonalExpenditure
#>                       1940   1945  1950 1955  1960
#> Food and Tobacco    22.200 44.500 59.60 73.2 86.80
#> Household Operation 10.500 15.500 29.00 36.5 46.20
#> Medical and Health   3.530  5.760  9.71 14.0 21.10
#> Personal Care        1.040  1.980  2.45  3.4  5.40
#> Private Education    0.341  0.974  1.80  2.6  3.64
# perform principal components analysis
(spend_pca <- prcomp(USPersonalExpenditure, center = FALSE))
#> Standard deviations (1, .., p=5):
#> [1] 78.04471215  3.95649695  1.26733701  0.18412188  0.04367521
#> 
#> Rotation (n x k) = (5 x 5):
#>             PC1         PC2        PC3         PC4         PC5
#> 1940 -0.1589586  0.11313761  0.1824780 -0.89728506 -0.35144462
#> 1945 -0.3016855  0.79223017 -0.5274149  0.02654943  0.04985844
#> 1950 -0.4293572  0.21081041  0.6202698 -0.01464040  0.62150007
#> 1955 -0.5323309  0.02659741  0.3024320  0.42107353 -0.66869157
#> 1960 -0.6449761 -0.56073415 -0.4607988 -0.12906349  0.20146975
# wrap the model as a `tbl_ord` object
(spend_pca <- as_tbl_ord(spend_pca))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # Rows: [ 5 x 5 | 0 ]
#>       PC1    PC2    PC3 ... | 
#>                             | 
#> 1 -137.    3.61  -0.310     | 
#> 2  -68.0  -5.35   1.48  ... | 
#> 3  -27.5  -4.45  -1.86      | 
#> 4   -7.11 -0.735 -0.795     | 
#> 5   -4.85 -0.782 -0.226     | 
#> # 
#> # Columns: [ 5 x 5 | 0 ]
#>      PC1     PC2    PC3 ... | 
#>                             | 
#> 1 -0.159  0.113   0.182     | 
#> 2 -0.302  0.792  -0.527 ... | 
#> 3 -0.429  0.211   0.620     | 
#> 4 -0.532  0.0266  0.302     | 
#> 5 -0.645 -0.561  -0.461     |
# augment the cases and variables with metadata
(spend_pca <- augment(spend_pca))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # Rows: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -137.    3.61  -0.310     | 1 Food and Tobacco   
#> 2  -68.0  -5.35   1.48  ... | 2 Household Operation
#> 3  -27.5  -4.45  -1.86      | 3 Medical and Health 
#> 4   -7.11 -0.735 -0.795     | 4 Personal Care      
#> 5   -4.85 -0.782 -0.226     | 5 Private Education  
#> # 
#> # Columns: [ 5 x 5 | 1 ]
#>      PC1     PC2    PC3 ... |   .name
#>                             |   <chr>
#> 1 -0.159  0.113   0.182     | 1 1940 
#> 2 -0.302  0.792  -0.527 ... | 2 1945 
#> 3 -0.429  0.211   0.620     | 3 1950 
#> 4 -0.532  0.0266  0.302     | 4 1955 
#> 5 -0.645 -0.561  -0.461     | 5 1960
# annotate the cases or variables
(spend_pca <- mutate_cols(spend_pca, year = as.integer(.name)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # Rows: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -137.    3.61  -0.310     | 1 Food and Tobacco   
#> 2  -68.0  -5.35   1.48  ... | 2 Household Operation
#> 3  -27.5  -4.45  -1.86      | 3 Medical and Health 
#> 4   -7.11 -0.735 -0.795     | 4 Personal Care      
#> 5   -4.85 -0.782 -0.226     | 5 Private Education  
#> # 
#> # Columns: [ 5 x 5 | 2 ]
#>      PC1     PC2    PC3 ... |   .name  year
#>                             |   <chr> <int>
#> 1 -0.159  0.113   0.182     | 1 1940   1940
#> 2 -0.302  0.792  -0.527 ... | 2 1945   1945
#> 3 -0.429  0.211   0.620     | 3 1950   1950
#> 4 -0.532  0.0266  0.302     | 4 1955   1955
#> 5 -0.645 -0.561  -0.461     | 5 1960   1960
# confer inertia equally between cases and variables
(spend_pca <- confer_inertia(spend_pca, c(.5, .5)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # Rows: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -11.0    1.28  -0.195     | 1 Food and Tobacco   
#> 2  -5.44  -1.90   0.929 ... | 2 Household Operation
#> 3  -2.20  -1.58  -1.17      | 3 Medical and Health 
#> 4  -0.569 -0.261 -0.499     | 4 Personal Care      
#> 5  -0.388 -0.278 -0.142     | 5 Private Education  
#> # 
#> # Columns: [ 5 x 5 | 2 ]
#>     PC1     PC2    PC3 ... |   .name  year
#>                            |   <chr> <int>
#> 1 -1.99  0.318   0.291     | 1 1940   1940
#> 2 -3.77  2.23   -0.840 ... | 2 1945   1945
#> 3 -5.36  0.593   0.988     | 3 1950   1950
#> 4 -6.65  0.0748  0.481     | 4 1955   1955
#> 5 -8.06 -1.58   -0.734     | 5 1960   1960
# render a biplot, by default on the first two shared coordinates
ggbiplot(spend_pca, aes(label = .name)) +
  theme_bw() +
  geom_cols_vector(aes(color = year)) +
  geom_cols_text_radiate() +
  geom_rows_point() +
  geom_rows_text_repel() +
  scale_y_continuous(expand = expand_scale(add = .5)) +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Symmetric biplot of un-centered PCA"
  )
#> Warning: `expand_scale()` is deprecated; use `expansion()` instead.
```

![](man/figures/README-PCA%20example-1.png)<!-- -->

### MDS example

Multidimensional scaling (MDS) is an ordination technique that starts
not with rectangular data but with interpoint distances. A common
illustration of its power is to calculate MDS on the set of
distances—however measured—between geographic locations, and to recover
the approximate geography via a biplot. This example is adapted from the
documentation of `cmdscale()` in the **stats** package; note that
**ordr** provides the wrapper `cmdscale_ord()` that always returns the
eigenvalues and the symmetric distance matrix produced during the
calculation. The MDS uses 11 coordinates—the number of positive
eigenvalues—so that `stat_*_spanningtree()` can call upon them to
recover the intercity distances.

``` r
# `tbl_ord` object for a classical MDS on distances between European cities
eurodist %>%
  cmdscale_ord(k = 11) %>%
  as_tbl_ord() %>%
  augment() %>%
  negate_ord(2) %>%
  print() -> city_mds
#> # A tbl_ord of class 'cmds_ord': (21 x 11) x (21 x 11)'
#> # 11 coordinates, transformed: PCo1, PCo2, ..., PCo11
#> # 
#> # Rows: [ 21 x 11 | 1 ]
#>     PCo1   PCo2   PCo3 ... |   .name    
#>                            |   <chr>    
#> 1 2290.  -1799.   53.8     | 1 Athens   
#> 2 -825.   -547. -114.  ... | 2 Barcelona
#> 3   59.2   367.  178.      | 3 Brussels 
#> 4  -82.8   430.  300.      | 4 Calais   
#> 5 -352.    291.  457.      | 5 Cherbourg
#> # … with 16 more rows
#> # 
#> # Columns: [ 21 x 11 | 1 ]
#>     PCo1   PCo2   PCo3 ... |   .name    
#>                            |   <chr>    
#> 1 2290.  -1799.   53.8     | 1 Athens   
#> 2 -825.   -547. -114.  ... | 2 Barcelona
#> 3   59.2   367.  178.      | 3 Brussels 
#> 4  -82.8   430.  300.      | 4 Calais   
#> 5 -352.    291.  457.      | 5 Cherbourg
#> # … with 16 more rows
# 2D biplot aligned with geography
city_mds %>%
  ggbiplot() +
  stat_cols_spantree(
    ord_aes(city_mds), check.aes = FALSE,
    alpha = .5, linetype = "dotted"
  ) +
  geom_cols_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of road distances between European cities")
```

![](man/figures/README-MDS%20example-1.png)<!-- -->

## acknowledgments

### contribute

Any feedback on the package is very welcome! If you encounter confusion
or errors, please create an issue with a reproducible example. If you
have requests, suggestions, or your own implementations for new
features, feel free to create an issue or submit a pull request. Methods
for additional ordination classes (see the `methods-*.r` scripts in the
`R` folder) are especially welcome, as are new plot layers. Above all,
be supportive!

### inspiration

This package was originally inspired by the **ggbiplot** extention
developed by [vqv](https://github.com/vqv/ggbiplot),
[richardjtelford](https://github.com/richardjtelford/ggbiplot), and
[GegnzaV](https://github.com/GegznaV/ggbiplot), among others. So far as
i know, it first brought biplots into the **tidyverse** framework. The
motivation to unify a variety of ordination methods came from [several
books and
articles](https://www.barcelonagse.eu/research/publications/all?author=Michael%20Greenacre)
by Michael Greenacre. Thomas Lin Pedersen’s
[**tidygraph**](https://github.com/thomasp85/tidygraph) sequel to
**ggraph** finally induced the shift from simply generating scatterplots
to upstream handling and manipulating ordination data.

[1] The term *ordination* is most prevalent among ecologists; to my
knowledge, no catch-all term is in common use outside ecology.

[2] Regression and clustering models, like classical [linear
regression](http://www.multivariatestatistics.org/chapter2.html) and
[*k*-means](http://joelcadwell.blogspot.com/2015/08/matrix-factorization-comes-in-many.html),
can also be understood as matrix decomposition approximations and even
visualized in biplots. Their shared coordinates, which are pre-defined
rather than artificial, are the predictor coefficients and the cluster
assignments, respectively. Methods for `stats::lm()` and
`stats::kmeans()`, for example, are implemented for the sake of novelty
and instruction, but are not widely used in practice.
