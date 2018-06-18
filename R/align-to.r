#' Align one ordination to another of the same subjects or variables
#' 
#' Depending on the method of ordination, its interpretation may not depend on 
#' the signs, the order, or the orientation of the coordinates. 
#' \code{negate_to}, \code{permute_to}, and \code{rotate_to} take advantage of 
#' these symmetries in order to manipulate the coordinates of one ordination so 
#' that the positions of the subjects or variables align, as closely as 
#' possible, to those of the same subjects or variables in another ordination. 
#' In the case of negation and permutation, this is done by minimizing the 
#' angles between vectors of scores or loadings in the respective coordinates. 
#' In the case of rotation, this is done by invoking the singular value 
#' decomposition method of point cloud registration (Bellekens &al, 2014).
#' 
#' @references
#' 
#' Bellekens B., Spruyt V., Berkvens R., & Weyn M. (2014). A survey of rigid 3D
#' pointcloud registration algorithms. \emph{Fourth International Conference on
#' Ambient Computing, Applications, Services and Technologies, Proceedings.} p.
#' 8--13.
#' 

#' @name align-to
#' @importFrom stats cor
#' @param x,y Matrices or bibbles; \code{x} will be aligned to \code{y}.
#' @template matrix-param
#' @param abs.values Whether \code{permute_to} should reorder coordinates
#'   according to dot product magnitudes (angles closest to straight, whether
#'   \eqn{0} or \eqn{\pi}, versus angles closest to \eqn{0}).
#' @param ... Additional parameters passed to methods.

#' @rdname align-to
#' @export
align_to <- function(x, y, .matrix) {
  # check that alignment is possible
  prev_class <- setdiff(class(x), "bbl")
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

#' @rdname align-to
#' @export
negate_to <- function(x, y, ...) UseMethod("negate_to")

#' @rdname align-to
#' @export
permute_to <- function(x, y, ...) UseMethod("permute_to")

#' @rdname align-to
#' @export
rotate_to <- function(x, y, ...) UseMethod("rotate_to")

#' @rdname align-to
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

#' @rdname align-to
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

#' @rdname align-to
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

attribute_alignment <- function(x, r) {
  #attr(x, "alignment") <- if (is.null(attr(x, "alignment"))) r else {
  #  attr(x, "alignment") %*% r
  #}
  attr(x, "alignment") <- r
  x
}

align_uv <- function(x, .matrix) {
  m <- get_factor(x, .matrix)
  r <- attr(x, "alignment")
  if (is.null(r)) return(m) else {
    return(m %*% r)
  }
}

#' @rdname align-to
#' @export
align_u <- function(x) align_uv(x, .matrix = "u")

#' @rdname align-to
#' @export
align_v <- function(x) align_uv(x, .matrix = "v")

#' @rdname align-to
#' @export
align_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = align_u(x),
    v = align_v(x),
    uv = list(u = align_u(x), v = align_v(x))
  )
}

#' @rdname align-to
#' @export
align_coord <- function(x) {
  if (is.null(attr(x, "alignment"))) {
    get_coord(x)
  } else {
    colnames(as.data.frame(matrix(1:dim(x), nrow = 1)))
  }
}

#' @rdname align-to
#' @export
un_align <- function(x) {
  attr(x, "alignment") <- NULL
  x
}
