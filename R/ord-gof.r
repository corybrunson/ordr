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
#' @param rank The maximum rank for which to compute statistics; the default,
#'   `NULL` computes statistics up to the rank of the model.
#' @returns A vector, matrix, or list of matrices of numeric goodness-of-fit
#'   statistics. If no items are found, a matrix will have zero rows.
#' @example inst/examples/ex-gof.r
NULL

#' @rdname goodness-of-fit
#' @export
ord_quality <- function(x, rank = NULL) {
  # check rank
  l <- recover_inertia(x)
  if (is.null(rank)) rank <- length(l) else check_rank(rank, length(l))
  # compute quality
  q <- cumsum(l[seq(rank)]) / sum(l)
  names(q) <- recover_coord(x)[seq(rank)]
  q
}

# TODO: Add argument (`j`? `items`?) to specify subsets of row/column indices;
# ensure that minimal computations are performed.

#' @rdname goodness-of-fit
#' @export
ord_adequacy <- function(x, .matrix, rank = NULL) {
  .matrix <- match_factor(.matrix)
  # check rank
  l <- recover_inertia(x)
  if (is.null(rank)) rank <- length(l) else check_rank(rank, length(l))
  # double back
  if (.matrix == "dims")
    return(lapply(c("rows", "cols"), ord_adequacy, x = x, rank = rank))
  # recover components
  f <- recover_factor(x, .matrix)[, seq(rank), drop = FALSE]
  # empty case
  if (nrow(f) == 0L) return(f)
  # remove (any) inertia
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  f <- t( t(f) / (sqrt(l[seq(rank)]) ^ p) )
  # array diagonal elements by rank
  if (rank == 1L) f^2 else t( apply( f^2, 1L, cumsum ) )
}

#' @rdname goodness-of-fit
#' @export
ord_predictivity <- function(x, .matrix, rank = NULL) {
  .matrix <- match_factor(.matrix)
  # check rank
  l <- recover_inertia(x)
  if (is.null(rank)) rank <- length(l) else check_rank(rank, length(l))
  # double back
  if (.matrix == "dims")
    return(lapply(c("rows", "cols"), ord_predictivity, x = x, rank = rank))
  # recover component
  f <- recover_factor(x, .matrix)
  # empty case
  if (nrow(f) == 0L) return(f[, seq(rank), drop = FALSE])
  # square and weight by full inertia with (any) existing inertia removed
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  flf <- t( t(f^2) * (l ^ (1 - p)) )
  # numerator: cumulative sums up to `rank`
  num <- t( apply( flf[, seq(rank), drop = FALSE], 1L, cumsum ) )
  # denominator: total sums
  denom <- apply( flf, 1L, sum )
  # quotient
  num / denom
}

check_rank <- function(rank, max_rank) {
  if (! is.numeric(rank) || rank <= 0)
    stop("`rank` must be a positive number.", call. = FALSE)
  if (rank > max_rank)
    stop("`rank` exceeds the rank of the model.", call. = FALSE)
}
