
<!-- edit README.rmd -->
ordr
====

**ordr** is designed to integrate ordination analysis and biplot visualization into a [**tidyverse**](https://github.com/tidyverse/tidyverse) workflow.

motivation
----------

### ordination and biplots

*Ordination* is a catch-all term for a variety of classical statistical (and machine learning) techniques that introduce an artificial coordinate scheme to a set of data [1]. In this way ordination and regression can be contrasted with clustering and classification, in that they assign continuous rather than discrete values to data elements.

Most ordination techniques decompose a numeric rectangular data set into the product of two (or more) matrices. In some cases, such as principal components analysis, the decomposition is exact; in others, such as non-negative matrix factorization, it is approximate; and in others, such as correspondence analysis, the data is transformed before decomposition. Ordination techniques may be supervised, like linear discriminant analysis, or unsupervised, like multidimensional scaling. Analysis pipelines that use these techniques may use the artifical coordinates directly, in place of natural coordinates, to arrange and compare data elements or to predict responses. They also frequently use *biplots* to represent unlike data elements on a shared set of axes.

The matrix decomposition underlying an ordination technique usually results in at least two matrices, say *U* and *V*, one corresponding to the rows and the other to the columns of the original data matrix *X*: If *X* has *r* rows and *c* columns, then *U* and *V* have *r* rows and *c* rows, respectively, and both have *d* columns, with *d* being the rank of the ordination [2]. These *d* columns provide a set of shared coordinates for the rows and columns of *X*. If *X* contains a measured value for each of several variables (columns) on each of several cases (rows), then the ordination provides a space in which both cases and variables can be situated. If *X* is a frequency table for two categorical variables, then the values of the two variables can be overlaid on the same *d* shared coordinates. Such a scatterplot visualization is called a biplot.

### implementations in R

An extensive range of ordination techniques are already implemented in R, from classical multidimensional scaling (`stats::cmdscale()`) and principal components analysis (`stats::prcomp()` and `stats::princomp()`) in the **stats** package distributed with base R, across widely-used implementations of linear discriminant analysis (`MASS::lda()`) and correspondence analysis (`ca::ca()`) in general-use statistical packages, to highly specialized packages that implement cutting-edge techniques or tailor conventional techniques to challenging settings. These implementations come with their own conventions, tailored to the research communities that produced them, and it would be impractical and probably even unhelpful to try to consolidate them.

Instead, **ordr** provides a streamlined process by which the outputs of these methods—in particular, the matrix factors into which the original data are approximately decomposed and the artificial coordinates they share—can be inspected, annotated, tidied, and visualized. On this last point the package may be especially helpful, since most biplot implementations provide limited customizability. The package is designed to follow the syntactic and grammatical conventions of the **tidyverse**, so that users familiar with a tidy workflow can more easily and quickly integrate ordination models into their practice.

usage
-----

### installation

**ordr** remains under development and is not scheduled for a CRAN release. For now, it can be installed using [**devtools**](https://github.com/r-lib/devtools):

``` r
devtools::install_github("corybrunson/ordr")
```

### example

Here is an example workflow that begins with a conventional PCA, proceeds through several methods provided for `tbl_ord` objects, and produces a biplot as a `ggplot` object:

``` r
USPersonalExpenditure
#>                       1940   1945  1950 1955  1960
#> Food and Tobacco    22.200 44.500 59.60 73.2 86.80
#> Household Operation 10.500 15.500 29.00 36.5 46.20
#> Medical and Health   3.530  5.760  9.71 14.0 21.10
#> Personal Care        1.040  1.980  2.45  3.4  5.40
#> Private Education    0.341  0.974  1.80  2.6  3.64
# perform principal components analysis
(pca <- prcomp(USPersonalExpenditure, center = FALSE))
#> Standard deviations:
#> [1] 78.04471215  3.95649695  1.26733701  0.18412188  0.04367521
#> 
#> Rotation:
#>             PC1         PC2        PC3         PC4         PC5
#> 1940 -0.1589586  0.11313761  0.1824780 -0.89728506 -0.35144462
#> 1945 -0.3016855  0.79223017 -0.5274149  0.02654943  0.04985844
#> 1950 -0.4293572  0.21081041  0.6202698 -0.01464040  0.62150007
#> 1955 -0.5323309  0.02659741  0.3024320  0.42107353 -0.66869157
#> 1960 -0.6449761 -0.56073415 -0.4607988 -0.12906349  0.20146975
# wrap the model as a `tbl_ord` object
(pca_ord <- as_tbl_ord(pca))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 0 ]
#>       PC1    PC2    PC3 ... | 
#>                             | 
#> 1 -137.    3.61  -0.310     | 
#> 2  -68.0  -5.35   1.48  ... | 
#> 3  -27.5  -4.45  -1.86      | 
#> 4   -7.11 -0.735 -0.795     | 
#> 5   -4.85 -0.782 -0.226     | 
#> # 
#> # V: [ 5 x 5 | 0 ]
#>      PC1     PC2    PC3 ... | 
#>                             | 
#> 1 -0.159  0.113   0.182     | 
#> 2 -0.302  0.792  -0.527 ... | 
#> 3 -0.429  0.211   0.620     | 
#> 4 -0.532  0.0266  0.302     | 
#> 5 -0.645 -0.561  -0.461     |
# augment the cases and variables with metadata
(pca_ord <- augment(pca_ord))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -137.    3.61  -0.310     | 1 Food and Tobacco   
#> 2  -68.0  -5.35   1.48  ... | 2 Household Operation
#> 3  -27.5  -4.45  -1.86      | 3 Medical and Health 
#> 4   -7.11 -0.735 -0.795     | 4 Personal Care      
#> 5   -4.85 -0.782 -0.226     | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 1 ]
#>      PC1     PC2    PC3 ... |   .name
#>                             |   <chr>
#> 1 -0.159  0.113   0.182     | 1 1940 
#> 2 -0.302  0.792  -0.527 ... | 2 1945 
#> 3 -0.429  0.211   0.620     | 3 1950 
#> 4 -0.532  0.0266  0.302     | 4 1955 
#> 5 -0.645 -0.561  -0.461     | 5 1960
# annotate the cases or variables
(pca_ord <- mutate_v(pca_ord, year = as.integer(.name)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -137.    3.61  -0.310     | 1 Food and Tobacco   
#> 2  -68.0  -5.35   1.48  ... | 2 Household Operation
#> 3  -27.5  -4.45  -1.86      | 3 Medical and Health 
#> 4   -7.11 -0.735 -0.795     | 4 Personal Care      
#> 5   -4.85 -0.782 -0.226     | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 2 ]
#>      PC1     PC2    PC3 ... |   .name  year
#>                             |   <chr> <int>
#> 1 -0.159  0.113   0.182     | 1 1940   1940
#> 2 -0.302  0.792  -0.527 ... | 2 1945   1945
#> 3 -0.429  0.211   0.620     | 3 1950   1950
#> 4 -0.532  0.0266  0.302     | 4 1955   1955
#> 5 -0.645 -0.561  -0.461     | 5 1960   1960
# confer inertia equally between cases and variables
(pca_ord <- confer_inertia(pca_ord, c(.5, .5)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>       PC1    PC2    PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -11.0    1.28  -0.195     | 1 Food and Tobacco   
#> 2  -5.44  -1.90   0.929 ... | 2 Household Operation
#> 3  -2.20  -1.58  -1.17      | 3 Medical and Health 
#> 4  -0.569 -0.261 -0.499     | 4 Personal Care      
#> 5  -0.388 -0.278 -0.142     | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 2 ]
#>     PC1     PC2    PC3 ... |   .name  year
#>                            |   <chr> <int>
#> 1 -1.99  0.318   0.291     | 1 1940   1940
#> 2 -3.77  2.23   -0.840 ... | 2 1945   1945
#> 3 -5.36  0.593   0.988     | 3 1950   1950
#> 4 -6.65  0.0748  0.481     | 4 1955   1955
#> 5 -8.06 -1.58   -0.734     | 5 1960   1960
# render a biplot, by default on the first two shared coordinates
ggbiplot(pca_ord, aes(label = .name)) +
  theme_bw() +
  geom_v_vector(aes(color = year)) +
  geom_v_text_radiate() +
  geom_u_point() +
  geom_u_text_repel() +
  scale_y_continuous(expand = expand_scale(add = .5)) +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Symmetric biplot of un-centered PCA"
  )
```

![](man/figures/README-example-1.png)

### caveat

It should be borne in mind that ordination is an enormous branch of statistics, in particular [geometric data analysis](https://www.springer.com/us/book/9781402022357). These techniques are often laden with a great deal of theoretical justification and domain-specific interpretation. This package is only meant to streamline the use of matrix factorization techniques in data analysis. See the documentation of the original techniques, and sources cited therein, for guidance on when and how to employ them.

acknowledgments
---------------

### contribute

Any feedback on the package is very welcome! If you encounter confusion or errors, please create an issue with an example.

This early version only handles a few types of biplot data, a few means of transforming and annotating them, and a few plot layers. My hope is that prospective contributors can see how to expand (and improve!) on the implementations thus far.

This package is open to all manner of input. Methods for additional ordination classes (see the `methods-*.r` scripts in the `R` folder) are especially welcome, as are new plot layers. If you think an overhaul of the package is in order, i encourage you to fork the repo and conduct it. Above all, be supportive!

### inspiration

This package was originally inspired by the **ggbiplot** extention developed by [vqv](https://github.com/vqv/ggbiplot), [richardjtelford](https://github.com/richardjtelford/ggbiplot), and [GegnzaV](https://github.com/GegznaV/ggbiplot), among others. So far as i know, it first brought biplots into the **tidyverse** framework. The motivation to unify a variety of ordination methods came from [several books and articles](https://www.barcelonagse.eu/research/publications/all?author=Michael%20Greenacre) by Michael Greenacre. Thomas Lin Pedersen's [**tidygraph**](https://github.com/thomasp85/tidygraph) sequel to **ggraph** finally induced the shift from simply generating scatterplots to upstream handling and manipulating ordination data.

### resources

-   Greenacre MJ (2010) [*Biplots in Practice*](http://www.multivariatestatistics.org/biplots.html).
-   Palmer M [*Ordination Methods for Ecologists*](http://ordination.okstate.edu/).
-   Podani J (2000) [*Introduction to the Exploration of Multivariate Biological Data*](http://ramet.elte.hu/~podani/books.html).
-   ttnphns (2015) [*Answer to* "PCA and Correspondence analysis in their relation to Biplot"](https://stats.stackexchange.com/a/141755/68743), CrossValidated.

[1] The term *ordination* is most prevalent among ecologists; to my knowledge, no catch-all term is in common use outside ecology.

[2] Regression and clustering models, like classical [linear regression](http://www.multivariatestatistics.org/chapter2.html) and [*k*-means](http://joelcadwell.blogspot.com/2015/08/matrix-factorization-comes-in-many.html), can also be understood as matrix decomposition approximations and visualized in biplots, on shared coordinates that are pre-defined rather than artificial. Methods for `stats::lm()` and `stats::kmeans()`, for example, are implemented for the sake of novelty and instruction, but are not widely used in practice.
