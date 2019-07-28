#' @title Functionality for singular value decompositions
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"svd"` returned by [svd_ord()].
#'
#' @name methods-svd
#' @include ord-tbl.r
#' @template param-methods
NULL

#' @rdname methods-svd
#' @export
as_tbl_ord.svd <- as_tbl_ord_default

#' @rdname methods-svd
#' @export
reconstruct.svd <- function(x) {
  x$u %*% diag(x$d) %*% t(x$v)
}

#' @rdname methods-svd
#' @export
recover_u.svd <- function(x) x[["u"]]

#' @rdname methods-svd
#' @export
recover_v.svd <- function(x) x[["v"]]

#' @rdname methods-svd
#' @export
recover_inertia.svd <- function(x) x$d ^ 2

#' @rdname methods-svd
#' @export
recover_coord.svd <- function(x) colnames(x[["u"]])

#' @rdname methods-svd
#' @export
recover_conference.svd <- function(x) {
  # `base::svd()` returns rotation matrices
  c(0, 0)
}

#' @rdname methods-svd
#' @export
augmentation_u.svd <- function(x) {
  .name <- rownames(x[["u"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["u"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augmentation_v.svd <- function(x) {
  .name <- rownames(x[["v"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["v"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augmentation_coord.svd <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .value = x$d[1:ncol(x$u)]
  )
}
