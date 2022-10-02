#' @title Negation of ordination axes
#'
#' @description Negate the coordinates of a subset of ordination axes in both
#'   row and column singular vectors.
#'   

#' @details
#'
#' For purposes of comparison and visualization, it can be useful to negate the
#' (already artificial) coordinates of an ordination, either by fixed criteria
#' or to better align with another basis (matrix) of coordinates. `negate_ord()`
#' allows the user to negate specified coordinates of an ordination.
#'
#' `get_negation()` accesses the negations of an ordination, an integer vector
#' of `1`s and `-1`s stored as a `"negate"` attribute.
#'

#' @name negation
#' @include ord-augmentation.r
#' @importFrom stats cor
#' @template param-matrix
#' @param x A [tbl_ord].
#' @param negation Integer vector of coordinates to negate.
#' @return `negate_ord()` and `negate_to_first_orthant()` return a tbl_ord with
#'   certain axes negated but the wrapped model unchanged. `get_negation()`
#'   returns the current negations. `revert_negation()` returns the tbl_ord
#'   without any manual negations.
#' @return A tbl_ord; the wrapped model is unchanged.
#' @example inst/examples/ex-ord-negation.r
NULL

attribute_negation <- function(x, r) {
  attr(x, "negate") <- r
  x
}

compose_negation <- function(x, r) {
  attr(x, "negate") <- if (is.null(attr(x, "negate"))) r else {
    attr(x, "negate") * r
  }
  x
}

#' @rdname negation
#' @export
get_negation <- function(x) {
  if (is.null(attr(x, "negate"))) {
    a <- rep(1L, length(recover_inertia(x)))
    names(a) <- recover_coord(x)
    return(a)
  } else {
    return(attr(x, "negate"))
  }
}

#' @rdname negation
#' @export
revert_negation <- function(x) attribute_negation(x, NULL)

#' @rdname negation
#' @export
negate_ord <- function(x, negation = NULL) {
  stopifnot(is_tbl_ord(x))
  if (is.null(negation)) return(x)
  if (is.character(negation)) {
    # interpret as a subset of coordinate names
    negation <- match(negation, get_coord(x))
  }
  if (all(negation %in% c(-1L, 1L)) & length(negation) > 1L) {
    # interpret as a vector of signs
    negation <- which(negation == -1L)
  }
  if (is.numeric(negation)) {
    # interpret as a subset of coordinates
    negation <- ifelse(seq_along(recover_inertia(x)) %in% negation, -1L, 1L)
  }
  names(negation) <- get_coord(x)
  compose_negation(x, negation)
}

#' @rdname negation
#' @export
negate_to_first_orthant <- function(x, .matrix) {
  # get matrices to align
  f <- get_factor(x, .matrix = .matrix)
  y <- matrix(1, nrow(f), ncol(f))
  # signs of dot products of matrix columns
  signs <- sign(apply(
    (f[, seq(ncol(f)), drop = FALSE] * y[, seq(ncol(f)), drop = FALSE]),
    2L, sum
  ))
  # extract ordinates to negate
  negs <- which(signs == -1L)
  # negate ordination
  negate_ord(x, signs)
}
