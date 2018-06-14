
align_to <- function(x, y, .matrix) {
  # check that alignment is possible
  prev_class <- setdiff(class(x), "bbl")
  if (any(prev_class) %in% method_classes("rotate_to")) {
    return(rotate_to(x, y, .matrix))
  } else {
    if (any(prev_class) %in% method_classes("permute_to")) {
      x <- permute_to(x, y, .matrix)
    }
    if (any(prev_class) %in% method_classes("negate_to")) {
      x <- negate_to(x, y, .matrix)
    }
    return(x)
  }
}

negate_to <- function(x, y, ...) UseMethod("negate_to")
permute_to <- function(x, y, ...) UseMethod("permute_to")
rotate_to <- function(x, y, ...) UseMethod("rotate_to")

negate_to.matrix <- function(x, y) {
  s <- negation_to(x, y)
  sweep(x, 2, s, "*")
}
permute_to.matrix <- function(x, y, abs.values = FALSE) {
  p <- permutation_to(x, y, abs.values = abs.values)
  x[, p, drop = FALSE]
}
rotate_to.matrix <- function(x, y) {
  m <- rotation_to(x, y)
  x %*% m
}

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

rotation_to <- function(x, y) {
  # https://biblio.ugent.be/publication/5713477
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
  attr(x, "alignment") <- if (is.null(attr(x, "alignment"))) r else {
    attr(x, "alignment") %*% r
  }
  x
}
