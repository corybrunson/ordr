#' @title **ordr** package
#'
#' @description This is a **[tidyverse](https://www.tidyverse.org/)** extension
#'   for handling, manipulating, and visualizing ordination models with
#'   consistent conventions and in a tidy workflow.
#'   

#' @details
#'
#' This package is designed to integrate ordination analysis and biplot
#' visualization into a **tidyverse** workflow. It is inspired in particular by
#' the extensions **ggbiplot** and **tidygraph**.
#' 
#' The package consists in several modules:
#' - the '[tbl_ord]' class, a wrapper for various ordination object classes
#' - extracting [augmentation] for the factors of an ordination
#' - using [dplyr-verbs] to add [annotation] to the factors
#' - adjusting the [conference] of inertia between the factors
#' - methods of the above generics for several widely-used object classes
#' - convenient [formatting][format] of ordination objects
#' - [ggbiplot()], a **ggplot2** extension for rendering biplots
#' - additional [stats][biplot-stats] and [geoms][biplot-geoms] for biplots

#' @section Ordinations and biplots:

#' _Ordination_ encompasses a variety of techniques for data compression,
#' dimension reduction, feature extraction, and visualization. Well-known
#' ordination techniques are predominantly unsupervised and include principal
#' components analysis, multidimensional scaling, and correspondence analyis
#' (Podani, 2000, Chapter 7; Palmer, n.d.). These methods are theoretically
#' grounded in geometric data analysis (Le Roux & Rouanet, 2004) and powered by
#' the matrix factorizations described below. A variety of other techniques may
#' also be viewed, or treated using the same tools, as ordination, including
#' linear regression, linear discriminant analysis, k-means clustering, and
#' non-negative matrix factorization.
#'
#' _Biplots_ are two-layered scatterplots widely used to visualize unsupervised
#' SVD-based ordinations. Gabriel (1971) introduced biplots to represent the
#' scores and loadings of PCA on a single set of axes. They have also been used
#' to visualize generalized linear regression and linear discriminant analysis
#' (Greenacre, 2010) and can adapted to any 2-factor matrix decomposition.
#'
#' 

#' @section Singular value decomposition:

#' The most popular ordination techniques use singular value decomposition (SVD)
#' to factor a data matrix \eqn{X} into a product \eqn{X=UDV'} of two orthogonal
#' (rotation) matrices \eqn{U} and \eqn{V} and a diagonal (scaling) matrix
#' \eqn{D}, with \eqn{V'} the transpose of \eqn{V}. In most cases, the data
#' matrix \eqn{X} is transformed from an original data matrix, e.g. by
#' centering, scaling, double-centering, or log-transforming. The SVD introduces
#' a set of shared orthogonal coordinates in which \eqn{U} encodes the rows of
#' \eqn{X} and \eqn{V} encodes the columns of \eqn{X}. The singular values in
#' \eqn{D} are the variances of \eqn{X} along each of these coordinates, and
#' they proceed in decreasing order, so that the first \eqn{r} (for "rank")
#' columns of \eqn{U} and of \eqn{V} produce a geometrically optimized
#' approximation to \eqn{X}.
#'
#' Biplots of SVD-based ordinations usually plot the rows and columns of \eqn{X}
#' on these \eqn{r} coordinate axes. For an SVD-based biplot to be truly
#' geometric, the total variance contained in \eqn{D} must be conferred onto
#' \eqn{U} or \eqn{V}, or distributed over both (Orlov, 2015). When \eqn{D} is
#' conferred onto \eqn{U}, the rows of \eqn{X} are represented by the rows of
#' \eqn{UD}, and their distances in the biplot approximate their distances in
#' the original column space of \eqn{X}. Meanwhile, the columns of \eqn{X} are
#' represented by the rows of \eqn{V}. These are unit vectors in the full space
#' of shared coordinates, so their squared lengths in the biplot indicate the
#' proportion of their variance captured by the biplot axes and their cosines
#' with each other approximate the correlations between the columns. Finally,
#' the projection of a row's coordinates (point) onto a column's coordinates
#' (vector) approximates the corresponding entry of \eqn{X}.
#' 

#' @template ref-podani2000
#' @template ref-palmer
#' @template ref-roux2004
#' @template ref-gabriel1971
#' @template ref-greenacre2010
#' @template ref-orlov2015

#' @name ordr
#' @aliases ordr-package
#' @keywords internal
#'

#' @section Acknowledgments:
#'

#' Many users have identified problems and suggested improvements.
#'
#' Development benefitted from the use of equipment and the support of
#' colleagues at [UConn Health](https://health.uconn.edu/) and at [UF
#' Health](https://ufhealth.org/).
#' 
"_PACKAGE"

if (getRversion() >= "2.15.1") utils::globalVariables(c(
  "."
))
