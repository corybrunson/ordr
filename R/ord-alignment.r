#' @title Alignment of ordinations
#'
#' @description Align one ordination to another having the same cases or
#'   variables.
#'   

#' @details
#'
#' For purposes of comparison and visualization, it can be useful to transform
#' the (already artificial) coordinates of an ordination, either by fixed
#' criteria or to better align with another basis (matrix) of coordinates.
#' `negate()`, `permute()`, and `rotate()` allow the user to manually transform
#' the coordinates of an ordination by specifying the coordinates to be negated
#' or permuted, or by providing the rotation matrix.
#'
#' `negate_to()`, `permute_to()`, and `rotate_to()` manipulate the coordinates
#' of one ordination so that the positions of the cases or variables align, as
#' closely as possible, to those of the same cases or variables in another
#' ordination. In the case of negation and permutation, this is done by
#' minimizing the angles between vectors of scores or loadings in the respective
#' coordinates. In the case of rotation, this is done by invoking the singular
#' value decomposition method of point cloud registration (Bellekens &al, 2014).
#'
#' The helper functions `negation_to()`, `permutation_to()`, and `rotation_to()`
#' take only matrix inputs and return matrices that perform the specified
#' transformations.
#'
#' `get_alignment()` accesses the alignment matrix of an ordination; after
#' multiple transformations, this matrix will be their composition (product).
#' `revert_alignment()` undoes all alignments. Currently alignments are stored
#' as an `"align"` attribute.
#'
#' @template ref-bellekens2014
#'   

#' @name alignment
#' @include ord-augmentation.r
#' @importFrom stats cor
#' @param x,y Matrices or [tbl_ord]s; `x` will be aligned to `y`.
#' @template param-matrix
#' @param negation,permutation,rotation Numeric or character vector of
#'   coordinates to negate or permute, or a matrix that performs an operation by
#'   right-multiplication.
#' @param method Character, one of `"negate"`, `"permute"`, or `"rotate"`,
#'   indicating the alignment method to use.
#' @param abs.values Whether `permute_to()` should reorder coordinates according
#'   to dot product magnitudes (angles closest to straight, whether \eqn{0} or
#'   \eqn{\pi}, versus angles closest to \eqn{0}).
#' @param ... Additional parameters passed to methods.
#' @example inst/examples/cities-cmds-align.r
#' @example inst/examples/country-cmds-prcomp-negate.r
#' @example inst/examples/haireye-ca-rotate.r
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
get_alignment <- function(x) {
  if (is.null(attr(x, "align"))) {
    a <- diag(dim(x))
    colnames(a) <- recover_coord(x)
    return(a)
  } else {
    return(attr(x, "align"))
  }
}

#' @rdname alignment
#' @export
revert_alignment <- function(x) attribute_alignment(x, NULL)

#' @rdname alignment
#' @export
negate <- function(x, negation = NULL) {
  stopifnot(is_tbl_ord(x))
  if (is.null(negation)) return(x)
  if (is.character(negation)) {
    negation <- match(negation, get_coord(x))
  }
  if (is.vector(negation)) {
    negation <- diag(ifelse(1:dim(x) %in% negation, -1, 1))
  }
  colnames(negation) <- get_coord(x, align = TRUE)
  compose_alignment(x, negation)
}

#' @rdname alignment
#' @export
permute <- function(x, permutation = NULL) {
  stopifnot(is_tbl_ord(x))
  if (is.null(permutation)) return(x)
  # ensure permutation matrix
  if (is.character(permutation)) {
    permutation <- match(permutation, get_coord(x))
  }
  if (is.vector(permutation)) {
    p <- c(permutation, setdiff(1:dim(x), permutation))
    permutation <- matrix(0, nrow = dim(x), ncol = dim(x))
    permutation[1:dim(x), p] <- 1
  }
  # permute coordinates
  p <- apply(permutation, 2, which.max)
  colnames(permutation) <- get_coord(x, align = TRUE)[p]
  compose_alignment(x, permutation)
}

#' @rdname alignment
#' @export
rotate <- function(x, rotation = NULL) {
  stopifnot(is_tbl_ord(x))
  if (is.null(rotation)) return(x)
  # ensure rotation matrix
  stopifnot(all.equal(rotation %*% t(rotation), diag(nrow(rotation))))
  stopifnot(all.equal(t(rotation) %*% rotation, diag(ncol(rotation))))
  # augment to dimensions of `x`
  d <- ncol(rotation)
  if (d < dim(x)) {
    rotation <- rbind(
      cbind(rotation, matrix(0, d, ncol(x) - d)),
      cbind(matrix(0, ncol(x) - d, d), diag(1, ncol(x) - d))
    )
  }
  compose_alignment(x, rotation)
}

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
  signs <- c(signs, rep(1, ncol(x) - d))
  # negation matrix
  diag(signs, nrow = ncol(x))
}

#' @rdname alignment
#' @export
negate_to <- function(x, y, .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(as.matrix(x, .matrix = .matrix), y)
  # retain coordinates
  colnames(s) <- colnames(as.matrix(x, .matrix = .matrix))
  # tag 'tbl_ord' object with negation
  attribute_alignment(x, s)
}

#' @rdname alignment
#' @export
negate_to_nonneg_orthant <- function(x, .matrix) {
  u <- get_factor(x, .matrix = .matrix)
  y <- matrix(1, nrow(u), ncol(u))
  negate_to(x, y, .matrix)
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
  # permutation matrix
  diag(1, nrow = d)[, inds, drop = FALSE]
}

#' @rdname alignment
#' @export
permute_to <- function(x, y, .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get permutation matrix
  p <- permutation_to(get_factor(as_tbl_ord(x), .matrix), y)
  # permute coordinates
  colnames(p) <-
    colnames(as.matrix(x, .matrix = .matrix))[apply(p, 2, which.max)]
  # tag 'tbl_ord' object with permutation
  attribute_alignment(x, p)
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
  # reduced-dimensional rotation matrix
  r <- s$u %*% t(s$v)
  # augment to dimensions of `x`
  if (d < ncol(x)) {
    r <- rbind(
      cbind(r, matrix(0, d, ncol(x) - d)),
      cbind(matrix(0, ncol(x) - d, d), diag(1, ncol(x) - d))
    )
  }
  # rotation matrix
  r
}

#' @rdname alignment
#' @export
rotate_to <- function(x, y, .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get rotation matrix
  r <- rotation_to(get_factor(as_tbl_ord(x), .matrix), y)
  # tag 'tbl_ord' object with rotation
  x <- attribute_alignment(x, r)
  # return annotated object
  x
}

#' @rdname alignment
#' @export
align_to <- function(x, y, .matrix, method = c("negate", "permute", "rotate")) {
  align_fun <- get(paste0(method, "_to"))
  align_fun(x, y, .matrix)
}
