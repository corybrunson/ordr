#' @title Classes for workhorse matrix decompositions
#' 
#' @description ...

#' @name classes
#' @include ord-tbl.r
#' @inheritParams base::eigen
#' @inheritParams base::svd
#' @example inst/examples/karate-igraph-eigen.r
NULL

#' @rdname classes
#' @export
eigen_ord <- function(x, symmetric = isSymmetric.matrix(x)) {
  res <- eigen(x = x, only.values = FALSE, EISPACK = FALSE)
  rownames(res$vectors) <- rownames(x)
  colnames(res$vectors) <- paste0("EV", seq_along(res$values))
  class(res) <- "eigen"
  res
}

#' @rdname classes
#' @export
svd_ord <- function(x, nu = min(dim(x)), nv = min(dim(x))) {
  res <- svd(x = x, nu = nu, nv = nv, LINPACK = FALSE)
  rownames(res$u) <- rownames(x)
  colnames(res$u) <- paste0("SV", seq_along(res$d))
  rownames(res$v) <- colnames(x)
  colnames(res$v) <- paste0("SV", seq_along(res$d))
  class(res) <- "svd"
  res
}
