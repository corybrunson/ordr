#' @title Measures of goodness of fit of ordination models
#'
#' @description Compute the quality, adequacy, and predictivity of a 'tbl_ord'
#'   object from the retrieved matrix factors.
#'
#' @details
#'
#' Gower, Gardner--Lubbe, & le Roux (2011) detail several measures of fit for
#' biplots, most prominently
#' \itemize{
#'   \item the _quality_ of the \eqn{r}-dimensional biplot,
#'         measured as the proportion of variance in the plot,
#'         calculated as the quotient of the traces of
#'         \eqn{\Lambda_r = {D_r}^2} and of \eqn{\Lambda = D^2}.
#'   \item the _adequacy_ of the representation of the \eqn{j}-th row
#'         (respectively, column) in the \eqn{r}-dimensional biplot,
#'         calculated as the \eqn{j}-th diagonal element of
#'         \eqn{U_r\ {U_r}^\top} (respectively, \eqn{V_r\ {V_r}^\top}),
#'         understood as the fidelity of the projections
#'         of the standard coordinates.
#'   \item the _predictivity_ of the \eqn{j}-th row
#'         (respectively, column) in the \eqn{r}-dimensional biplot,
#'         measured as the quotient of the \eqn{j}-th diagonal elements of
#'         \eqn{U_r\ \Lambda_r\ {U_r}^\top} and of \eqn{U\ \Lambda\ U^\top}
#'         (respectively, of \eqn{V_r\ \Lambda_r\ {V_r}^\top}
#'         and of \eqn{V\ \Lambda\ V^\top}),
#'         understood as the fidelity of the projections
#'         of the principal coordinates.
#' }
#' These can be calculated directly from any SVD or EVD and interpreted for any
#' technique based on them. In some cases they may also be calculated for
#' supplementary elements.

#' @template ref-gower2011

#' @name goodness-of-fit
#' @aliases gof
#' @include ord-tbl.r
#' @param x A 'tbl_ord' object.
#' @template param-matrix
#' @returns A vector, matrix, or list of matrices of numeric goodness-of-fit
#'   statistics.
#' @example inst/examples/ex-gof.r
NULL

#' @rdname goodness-of-fit
#' @export
ord_quality <- function(x) {
  l <- recover_inertia(x)
  q <- cumsum(l) / sum(l)
  names(q) <- recover_coord(x)
  q
}

# TODO: Add arguments (`j` and `r`? `items` and `rank`?) to specify (subsets of)
# row/column indices and biplot dimensions and ensure that minimal computations
# are performed.

#' @rdname goodness-of-fit
#' @export
ord_adequacy <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  # double up
  if (.matrix == "dims")
    return(lapply(c("rows", "cols"), ord_adequacy, x = x))
  # recover components
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  f <- recover_factor(x, .matrix)
  l <- recover_inertia(x)
  # remove (any) inertia
  f <- t( t(f) / (l^p) )
  # array diagonal elements by rank
  t( apply( f^2, 1L, cumsum ) )
}

#' @rdname goodness-of-fit
#' @export
ord_predictivity <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  # double up
  if (.matrix == "dims")
    return(lapply(c("rows", "cols"), ord_predictivity, x = x))
  # recover components
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  f <- recover_factor(x, .matrix)
  l <- recover_inertia(x)
  # remove (any) inertia
  f <- t( t(f) / (l^p) )
  # array diagonal elements by rank
  flf_ <- t( apply( t(f^2) * sqrt(l), 2L, cumsum ) )
  flf_ / flf_[, length(l)]
}
