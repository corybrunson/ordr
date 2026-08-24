# **ordr** package

This is a **[tidyverse](https://www.tidyverse.org/)** extension for
handling, manipulating, and visualizing ordination models with
consistent conventions and in a tidy workflow.

## Details

This package is designed to integrate ordination analysis and biplot
visualization into a **tidyverse** workflow. It is inspired in
particular by the extensions **ggbiplot** and **tidygraph**.

The package consists in several modules:

- the '[tbl_ord](tbl_ord.md)' class, a wrapper for various ordination
  object classes

- extracting [augmentation](augmentation.md) for the factors of an
  ordination

- using [dplyr-verbs](dplyr-verbs.md) to add [annotation](annotation.md)
  to the factors

- adjusting the [conference](conference.md) of inertia between the
  factors

- methods of the above generics for several widely-used object classes

- convenient [formatting](format.md) of ordination objects

- [`ggbiplot()`](ggbiplot.md), a **ggplot2** extension for rendering
  biplots

- additional [stats](biplot-stats.md) and [geoms](biplot-geoms.md) for
  biplots

## Ordinations and biplots

*Ordination* encompasses a variety of techniques for data compression,
dimension reduction, feature extraction, and visualization. Well-known
ordination techniques are predominantly unsupervised and include
principal components analysis, multidimensional scaling, and
correspondence analyis (Podani, 2000, Chapter 7; Palmer, n.d.). These
methods are theoretically grounded in geometric data analysis (Le Roux &
Rouanet, 2004) and powered by the matrix factorizations described below.
A variety of other techniques may also be viewed, or treated using the
same tools, as ordination, including linear regression, linear
discriminant analysis, k-means clustering, and non-negative matrix
factorization.

*Biplots* are two-layered scatterplots widely used to visualize
unsupervised SVD-based ordinations. Gabriel (1971) introduced biplots to
represent the scores and loadings of PCA on a single set of axes. They
have also been used to visualize generalized linear regression and
linear discriminant analysis (Greenacre, 2010) and can adapted to any
2-factor matrix decomposition.

## Singular value decomposition

The most popular ordination techniques use singular value decomposition
(SVD) to factor a data matrix \\X\\ into a product \\X=UDV'\\ of two
orthogonal (rotation) matrices \\U\\ and \\V\\ and a diagonal (scaling)
matrix \\D\\, with \\V'\\ the transpose of \\V\\. In most cases, the
data matrix \\X\\ is transformed from an original data matrix, e.g. by
centering, scaling, double-centering, or log-transforming. The SVD
introduces a set of shared orthogonal coordinates in which \\U\\ encodes
the rows of \\X\\ and \\V\\ encodes the columns of \\X\\. The singular
values in \\D\\ are the variances of \\X\\ along each of these
coordinates, and they proceed in decreasing order, so that the first
\\r\\ (for "rank") columns of \\U\\ and of \\V\\ produce a geometrically
optimized approximation to \\X\\.

Biplots of SVD-based ordinations usually plot the rows and columns of
\\X\\ on these \\r\\ coordinate axes. For an SVD-based biplot to be
truly geometric, the total variance contained in \\D\\ must be conferred
onto \\U\\ or \\V\\, or distributed over both (Orlov, 2015). When \\D\\
is conferred onto \\U\\, the rows of \\X\\ are represented by the rows
of \\UD\\, and their distances in the biplot approximate their distances
in the original column space of \\X\\. Meanwhile, the columns of \\X\\
are represented by the rows of \\V\\. These are unit vectors in the full
space of shared coordinates, so their squared lengths in the biplot
indicate the proportion of their variance captured by the biplot axes
and their cosines with each other approximate the correlations between
the columns. Finally, the projection of a row's coordinates (point) onto
a column's coordinates (vector) approximates the corresponding entry of
\\X\\.

## Acknowledgments

Many users have identified problems and suggested improvements.

Development benefitted from the use of equipment and the support of
colleagues at [UConn Health](https://health.uconn.edu/) and at [UF
Health](https://ufhealth.org/).

## References

Podani J (2000) "Ordination". *Introduction to the Exploration of
Multivariate Biological Data* Chapter 7, 215–284. Backhuys Publishers,
ISBN 90-5782-067-6.
<https://web.archive.org/web/20200221000313/http://ramet.elte.hu/~podani/books.html>

Palmer M *Ordination Methods for Ecologists*. Website, accessed
2019-07-12. <https://ordination.okstate.edu/>

Le Roux B & Rouanet H (2004) *Geometric Data Analysis: From
Correspondence Analysis to Stsructured Data Analysis*. Springer
Dordrecht, ISBN: 978-1-4020-2236-4.
[doi:10.1007/1-4020-2236-0](https://doi.org/10.1007/1-4020-2236-0)
<https://link.springer.com/book/10.1007/1-4020-2236-0>

Gabriel KR (1971) "The biplot graphic display of matrices with
application to principal component analysis". *Biometrika* 58(3),
453–467.
[doi:10.1093/biomet/58.3.453](https://doi.org/10.1093/biomet/58.3.453)

Greenacre MJ (2010) *Biplots in Practice*. Fundacion BBVA, ISBN:
978-84-923846.
<https://www.fbbva.es/microsite/multivariate-statistics/biplots.html>

Orlov K (2015) *Answer to* "PCA and Correspondence analysis in their
relation to Biplot". CrossValidated, accessed 2019-07-12.
<https://stats.stackexchange.com/a/141755/68743>

## See also

Useful links:

- <https://github.com/corybrunson/ordr>

- <https://corybrunson.github.io/ordr/>

- Report bugs at <https://github.com/corybrunson/ordr/issues>

## Author

**Maintainer**: Jason Cory Brunson <cornelioid@gmail.com>
([ORCID](https://orcid.org/0000-0003-3126-9494))

Authors:

- John Gracey <jbgracey6@gmail.com>

Other contributors:

- Emily Paul <erpb.71@gmail.com> \[contributor\]
