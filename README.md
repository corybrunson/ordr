
<!-- edit README.rmd -->
tidybiplot
==========

**tidybiplot** is designed to integrate ordination analysis and biplot visualization into a [**tidyverse**](https://github.com/tidyverse/tidyverse) workflow.

motivation
----------

*Ordination* is a catch-all term for a variety of classical statistical (and machine learning) techniques that decompose a numeric rectangular data set into the product of two (or more) matrices. In some cases, such as principal components analysis, the decomposition is exact; in others, such as non-negative matrix factorization, it is approximate; and in others, such as multidimensional scaling and corresppondence analysis, the data is transformed before the decomposition is performed. What analyses that use these tools share is the inspection, and often the prediction and/or visualization, of two unlike data elements on a common set of coordinates.

The matrix decomposition usually results in at least two matrices, say *U* and *V*, one corresponding to the rows and the other to the columns of the original data matrix *X*: If *X* has *r* rows and *c* columns, then *U* and *V* have *r* rows and *c* rows, respectively, and both have *d* columns, with *d* being the rank of the ordination. These *d* columns provide a set of shared coordinates for the rows and columns of *X*. If *X* contains a measured value for each of several variables (columns) on each of several cases (rows), then the ordination provides a space in which both cases and variables can be situated. If *X* is a frequency table for two categorical variables, then the values of the two variables can be overlaid on the same *d* shared coordinates. Such a scatterplot visualization of values of two unlike types is called a *biplot*, and it is the key visualization method made possible by ordination.

An extensive range of ordination techniques are already implemented in R, from classical multidimensional scaling (`stats::cmdscale()`) and principal components analysis (`stats::prcomp()` and `stats::princomp()`) in the **stats** package distributed with base R, across widely-used implementations of linear discriminant analysis (`MASS::lda()`) and correspondence analysis (`ca::ca()`) in general-use statistical packages, to highly specialized packages that implement cutting-edge techniques or tailor conventional techniques to difficult settings. These implementations come with their own conventions, tailored to the research communities that produced them, and it would be unweildy and probably even unhelpful to try to consolidate them.

Rather, **tidybiplot** provides a streamlined process by which the outputs of these methods---in particular, the matrix factors into which the original data are approximately decomposed and the artificial coordinates they share---can be inspected, annotated, tidied, and visualized. On this last point the package may be especially helpful, since most biplot implementations provide limited customizability. The package is designed to follow the syntactic and grammatical conventions of the **tidyverse**, so that users familiar with a tidy workflow can more easily and quickly integrate ordination models into their practice.

usage
-----

### installation

**tidybiplot** remains under development and is not scheduled for a CRAN release. For now, it can be installed using [**devtools**](https://github.com/r-lib/devtools):

``` r
devtools::install_bitbucket("corybrunson/tidybiplot")
#> Downloading bitbucket repo corybrunson/tidybiplot@master
#> Installation failed: Unauthorized (HTTP 401).
```

### example

Here is an example workflow that begins with a conventional PCA, proceeds through several methods provided for `tbl_ord` objects, and produces a biplot as a `ggplot` object:

``` r
# perform principal components analysis
(pca <- prcomp(USPersonalExpenditure, scale = TRUE))
#> Standard deviations:
#> [1] 2.230797e+00 1.469298e-01 4.188265e-02 1.427097e-02 4.773889e-17
#> 
#> Rotation:
#>             PC1         PC2         PC3         PC4         PC5
#> 1940 -0.4481771 -0.03597112  0.38540616 -0.79065333  0.15547582
#> 1945 -0.4447746  0.84427905 -0.28905725  0.06494971 -0.03994831
#> 1950 -0.4480901 -0.11161654  0.53476542  0.40573614 -0.57979407
#> 1955 -0.4480367 -0.21494980  0.05684804  0.43830962  0.74680164
#> 1960 -0.4469801 -0.47669412 -0.69188311 -0.11794789 -0.28347407
# wrap the model as a `tbl_ord` object
(pca_ord <- as_tbl_ord(pca))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 0 ]
#>      PC1     PC2      PC3 ... | 
#>                               | 
#> 1 -3.60   0.112  -0.00458     | 
#> 2 -0.678 -0.231   0.0332  ... | 
#> 3  0.918 -0.0625 -0.0707      | 
#> 4  1.63   0.0963  0.0181      | 
#> 5  1.73   0.0851  0.0239      | 
#> # 
#> # V: [ 5 x 5 | 0 ]
#>      PC1     PC2     PC3 ... | 
#>                              | 
#> 1 -0.448 -0.0360  0.385      | 
#> 2 -0.445  0.844  -0.289  ... | 
#> 3 -0.448 -0.112   0.535      | 
#> 4 -0.448 -0.215   0.0568     | 
#> 5 -0.447 -0.477  -0.692      |
# augment the cases and variables with metadata
(pca_ord <- augment(pca_ord))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>      PC1     PC2      PC3 ... |   .name              
#>                               |   <chr>              
#> 1 -3.60   0.112  -0.00458     | 1 Food and Tobacco   
#> 2 -0.678 -0.231   0.0332  ... | 2 Household Operation
#> 3  0.918 -0.0625 -0.0707      | 3 Medical and Health 
#> 4  1.63   0.0963  0.0181      | 4 Personal Care      
#> 5  1.73   0.0851  0.0239      | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 3 ]
#>      PC1     PC2     PC3 ... |   .name .center .scale
#>                              |   <chr>   <dbl>  <dbl>
#> 1 -0.448 -0.0360  0.385      | 1 1940     7.52   9.14
#> 2 -0.445  0.844  -0.289  ... | 2 1945    13.7   18.1 
#> 3 -0.448 -0.112   0.535      | 3 1950    20.5   24.5 
#> 4 -0.448 -0.215   0.0568     | 4 1955    25.9   29.8 
#> 5 -0.447 -0.477  -0.692      | 5 1960    32.6   34.8
# annotate the cases or variables
(pca_ord <- mutate_v(pca_ord, year = as.integer(.name)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>      PC1     PC2      PC3 ... |   .name              
#>                               |   <chr>              
#> 1 -3.60   0.112  -0.00458     | 1 Food and Tobacco   
#> 2 -0.678 -0.231   0.0332  ... | 2 Household Operation
#> 3  0.918 -0.0625 -0.0707      | 3 Medical and Health 
#> 4  1.63   0.0963  0.0181      | 4 Personal Care      
#> 5  1.73   0.0851  0.0239      | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 4 ]
#>      PC1     PC2     PC3 ... |   .name .center .scale  year
#>                              |   <chr>   <dbl>  <dbl> <int>
#> 1 -0.448 -0.0360  0.385      | 1 1940     7.52   9.14  1940
#> 2 -0.445  0.844  -0.289  ... | 2 1945    13.7   18.1   1945
#> 3 -0.448 -0.112   0.535      | 3 1950    20.5   24.5   1950
#> 4 -0.448 -0.215   0.0568     | 4 1955    25.9   29.8   1955
#> 5 -0.447 -0.477  -0.692      | 5 1960    32.6   34.8   1960
# confer inertia equally between cases and variables
(pca_ord <- confer_inertia(pca_ord, c(.5, .5)))
#> # A tbl_ord of class 'prcomp': (5 x 5) x (5 x 5)'
#> # 5 coordinates: PC1, PC2, ..., PC5
#> # 
#> # U: [ 5 x 5 | 1 ]
#>      PC1    PC2     PC3 ... |   .name              
#>                             |   <chr>              
#> 1 -1.70   0.207 -0.0158     | 1 Food and Tobacco   
#> 2 -0.321 -0.426  0.115  ... | 2 Household Operation
#> 3  0.434 -0.115 -0.244      | 3 Medical and Health 
#> 4  0.770  0.178  0.0625     | 4 Personal Care      
#> 5  0.820  0.157  0.0827     | 5 Private Education  
#> # 
#> # V: [ 5 x 5 | 4 ]
#>      PC1     PC2     PC3 ... |   .name .center .scale  year
#>                              |   <chr>   <dbl>  <dbl> <int>
#> 1 -0.947 -0.0195  0.112      | 1 1940     7.52   9.14  1940
#> 2 -0.939  0.458  -0.0837 ... | 2 1945    13.7   18.1   1945
#> 3 -0.946 -0.0605  0.155      | 3 1950    20.5   24.5   1950
#> 4 -0.946 -0.117   0.0165     | 4 1955    25.9   29.8   1955
#> 5 -0.944 -0.258  -0.200      | 5 1960    32.6   34.8   1960
# render a biplot, by default on the first two shared coordinates for `x` and `y`
ggbiplot(pca_ord, aes(label = .name)) +
  theme_bw() +
  geom_u_point() +
  geom_u_text_repel() +
  geom_v_vector(aes(color = year)) +
  geom_v_text_radiate() +
  ggtitle(
    "PCA of U.S. Personal Expenditure data, 1940-1960",
    "Symmetric biplot"
  )
```

![](man/figures/README-example-1.png)

acknowledgments
---------------

### contribute

Any feedback on the package is very welcome! If you encounter confusion or errors, please create an issue with an example.

This early version only handles a few types of biplot data, a few means of transforming and annotating them, and a few plot layers. My hope is that prospective contributors can see how to expand (and improve!) on the implementations thus far.

Contributing guidelines will be posted soon. In the meantime, please respect the [**tidyverse** contributing guidelines](https://www.tidyverse.org/articles/2017/08/contributing/) and be supportive!

### inspiration

This package was originally inspired by the **ggbiplot** extention developed by [vqv](https://github.com/vqv/ggbiplot), [richardjtelford](https://github.com/richardjtelford/ggbiplot), and [GegnzaV](https://github.com/GegznaV/ggbiplot), among others. So far as i know, it first brought biplots into the **tidyverse** framework. The motivation to unify a variety of ordination methods came from [several books and articles](https://www.barcelonagse.eu/research/publications/all?author=Michael%20Greenacre) by Michael Greenacre. Thomas Lin Pedersen's [**tidygraph**](https://github.com/thomasp85/tidygraph) sequel to **ggraph** finally induced the shift from simply generating scatterplots to upstream handling and manipulating ordination data.

### resources

-   Greenacre MJ (2010) *Biplots in Practice*.
-   Palmer M ()
-   Podani J (2000) *Introduction to the Exploration of Multivariate Biological Data*.
-   ttnphns (2015) *Answer to* "PCA and Correspondence analysis in their relation to Biplot", CrossValidated.
