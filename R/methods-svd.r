#' Functionality for singular value decompositions
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"svd"}. (This is a class introduced in this package to identify 
#' objects returned by \code{\link{svd}}, which is masked by a wrapper that
#' adds the class attribute.)
#' 
#' @name methods-svd
#' @template param-methods

#' @rdname methods-svd
#' @export
as_tbl_ord.svd <- as_tbl_ord_default

#' @rdname methods-svd
#' @export
recover_u.svd <- function(x) {
  res <- x$u
  colnames(res) <- recover_coord.svd(x)
  rownames(res) <- rownames(attr(x, "x"))
  res
}

#' @rdname methods-svd
#' @export
recover_v.svd <- function(x) {
  res <- x$v
  colnames(res) <- recover_coord.svd(x)
  rownames(res) <- colnames(attr(x, "x"))
  res
}

#' @rdname methods-svd
#' @export
recover_inertia.svd <- function(x) x$d

#' @rdname methods-svd
#' @export
recover_coord.svd <- function(x) paste0("SV", 1:length(x$d))

#' @rdname methods-svd
#' @export
recover_conference.svd <- function(x) {
  # `base::svd()` returns rotation matrices
  c(0, 0)
}

#' @rdname methods-svd
#' @export
augment_u.svd <- function(x) {
  .name <- rownames(attr(x, "x"))
  if (is.null(.name)) {
    tibble_pole(nrow(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augment_v.svd <- function(x) {
  .name <- colnames(attr(x, "x"))
  if (is.null(.name)) {
    tibble_pole(ncol(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augment_coord.svd <- function(x) {
  tibble(
    .name = recover_coord(x),
    .value = x$d[1:ncol(x$u)]
  )
}

#' @rdname methods-svd
#' @export
reconstruct.svd <- function(x) {
  x$u %*% diag(x$d) %*% t(x$v)
}
