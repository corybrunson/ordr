#' @title Classes for workhorse matrix decompositions
#' 
#' @description ...

#' @name classes
#' @include ord-tbl.r
#' @inheritParams base::eigen
#' @inheritParams base::svd
NULL
# -+-INSTEAD OF ADDING `x` AS AN ATTRIBUTE, ASSIGN ROW AND COLUMN NAMES-+-

#' @rdname classes
#' @export
eigen_ord <- function(x, EISPACK = FALSE) {
  res <- eigen(x = x, only.values = FALSE, EISPACK = EISPACK)
  rownames(res$vectors) <- rownames(x)
  colnames(res$vectors) <- paste0("EV", seq_along(res$values))
  class(res) <- "eigen"
  res
}

#' @rdname classes
#' @export
svd_ord <- function(x, LINPACK = FALSE) {
  res <- svd(x = x, nu = min(dim(x)), nv = min(dim(x)), LINPACK = LINPACK)
  rownames(res$u) <- rownames(x)
  colnames(res$u) <- paste0("SV", seq_along(res$d))
  rownames(res$v) <- colnames(x)
  colnames(res$v) <- paste0("SV", seq_along(res$d))
  class(res) <- "svd"
  res
}
