#' @title Measures of goodness of fit of ordination models
#'
#' @description Compute the quality, adequacy, and predictivity of a 'tbl_ord'
#'   object from the retrieved matrix factors.
#'
#' @details
#'
#' (TODO: Add details from #44.)
#'
#' @include ord-tbl.r
#' @param x A 'tbl_ord' object.
#' @returns A vector or matrix of numeric goodness-of-fit statistics.
#' @example inst/examples/ex-gof.r

#' @export
ord_quality <- function(x) {
  l <- recover_inertia(x)
  q <- cumsum(l) / sum(l)
  names(q) <- recover_coord(x)
  q
}

# TODO: Add arguments (`j` and `r`?) to specify (subsets of) row/column indices
# and biplot dimensions.

#' @export
ord_adequacy <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  f <- recover_factor(x, .matrix)
  # remove (any) inertia
  l <- recover_inertia(x)
  f <- t( t(f) / (l^p) )
  # array diagonal elements by rank
  t( apply( f^2, 1L, cumsum ) )
}

#' @export
ord_predictivity <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  p <- recover_conference(x)[match(.matrix, c("rows", "cols"))]
  f <- recover_factor(x, .matrix)
  # remove (any) inertia
  l <- recover_inertia(x)
  f <- t( t(f) / (l^p) )
  # array diagonal elements by rank
  flf_ <- t( apply( t(f^2) * sqrt(l), 2L, cumsum ) )
  flf_ / flf_[, length(l)]
}
