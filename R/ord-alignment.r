#' @title Alignment of ordinations
#'
#' @description Align one ordination to another having the same cases or
#'   variables.
#'   

#' @details
#'
#' Depending on the method of ordination, its interpretation may not depend on
#' the signs, the order, or the orientation of the coordinates. `negate_to()`,
#' `permute_to()`, and `rotate_to()` take advantage of these symmetries in order
#' to manipulate the coordinates of one ordination so that the positions of the
#' cases or variables align, as closely as possible, to those of the same cases
#' or variables in another ordination. In the case of negation and permutation,
#' this is done by minimizing the angles between vectors of scores or loadings
#' in the respective coordinates. In the case of rotation, this is done by
#' invoking the singular value decomposition method of point cloud registration
#' (Bellekens &al, 2014).
#' 
#' @template ref-bellekens2014
#' 

#' @name alignment
#' @include ord-augmentation.r
#' @importFrom stats cor
#' @param x,y Matrices or `tbl_ord`s; `x` will be aligned to `y`.
#' @template param-matrix
#' @param coordinates Numeric or character vector of coordinates to negate or
#'   permute.
#' @param abs.values Whether `permute_to` should reorder coordinates according
#'   to dot product magnitudes (angles closest to straight, whether \eqn{0} or
#'   \eqn{\pi}, versus angles closest to \eqn{0}).
#' @param ... Additional parameters passed to methods.
#' @example inst/examples/country-cmds-prcomp-align.r
NULL

attribute_alignment <- function(x, r) {
  attr(x, "align") <- r
  x
}

compose_alignment <- function(x, r) {
  attr(x, "align") <- if (is.null(attr(x, "align"))) r else {
    attr(x, "align") %*% r
  }
  x
}

#' @rdname alignment
#' @export
negate <- function(x, coordinates = NULL) {
  if (is.character(coordinates)) {
    coordinates <- match(coordinates, recover_coord(x))
  }
  m <- diag(ifelse(1:dim(x) %in% coordinates, -1, 1))
  compose_alignment(x, m)
}

#' @rdname alignment
#' @export
permute <- function(x, coordinates = NULL) {
  if (is.character(coordinates)) {
    coordinates <- match(coordinates, recover_coord(x))
  }
  coordinates <- c(coordinates, setdiff(1:dim(x), coordinates))
  m <- matrix(0, nrow = dim(x), ncol = dim(x))
  m[1:dim(x), coordinates]
  compose_alignment(x, m)
}

#' @rdname alignment
#' @export
align_to <- function(x, y, .matrix) {
  # check that alignment is possible
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("rotate_to"))) {
    return(rotate_to(x, y, .matrix = .matrix))
  } else {
    if (any(prev_class %in% method_classes("permute_to"))) {
      x <- permute_to(x, y, .matrix = .matrix)
    }
    if (any(prev_class %in% method_classes("negate_to"))) {
      x <- negate_to(x, y, .matrix = .matrix)
    }
    return(x)
  }
}

#' @rdname alignment
#' @export
negate_to <- function(x, y, ...) UseMethod("negate_to")

#' @rdname alignment
#' @export
permute_to <- function(x, y, ...) UseMethod("permute_to")

#' @rdname alignment
#' @export
rotate_to <- function(x, y, ...) UseMethod("rotate_to")

#' @rdname alignment
#' @export
negation_to <- function(x, y) {
  stopifnot(nrow(x) == nrow(y))
  d <- min(ncol(x), ncol(y))
  # signs of dot products of matrix columns
  signs <- sign(apply(
    (x[, 1:d, drop = FALSE] * y[, 1:d, drop = FALSE]),
    2, sum
  ))
  # augment with `1`s
  c(signs, rep(1, ncol(x) - d))
}

#' @rdname alignment
#' @export
permutation_to <- function(x, y, abs.values = FALSE) {
  stopifnot(nrow(x) == nrow(y))
  d <- min(ncol(x), ncol(y))
  # match columns by dot product in order
  inds <- 1:d
  for (i in 1:d) {
    cosines <- t(x[, i:d, drop = FALSE]) %*%
      y[, i, drop = FALSE] /
      apply(x[, i:d, drop = FALSE], 2, function(x) sum(x^2)) /
      sum(y[, i] ^ 2)
    if (abs.values) cosines <- abs(cosines)
    j <- which.max(cosines) + (i - 1)
    if (j != i) {
      x[, c(i, j)] <- x[, c(j, i)]
      inds[c(i, j)] <- inds[c(j, i)]
    }
  }
  inds
}

#' @rdname alignment
#' @export
rotation_to <- function(x, y) {
  stopifnot(nrow(x) == nrow(y))
  d <- min(ncol(x), ncol(y))
  # cross correlation matrix between `x` and `y` based on matched points
  m <- cor(x[, 1:d, drop = FALSE], y[, 1:d, drop = FALSE])
  # SVD
  s <- svd(m)
  # rotation matrix
  r <- s$u %*% t(s$v)
  # augment to dimensions of `x`
  if (d < ncol(x)) {
    r <- rbind(
      cbind(r, matrix(0, d, ncol(x) - d)),
      cbind(matrix(0, ncol(x) - d, d), diag(1, ncol(x) - d))
    )
  }
  r
}

#' @rdname alignment
#' @export
un_align <- function(x) {
  attr(x, "align") <- NULL
  x
}
