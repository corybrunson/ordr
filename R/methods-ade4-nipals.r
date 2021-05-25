#' @title Functionality for non-linear iterative PLS ('nipals') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"nipals"` as returned by [ade4::nipals()].
#'
#' @name methods-ade4
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-ade4-airquality.r
NULL

#' @rdname methods-ade4
#' @export
as_tbl_ord.nipals <- as_tbl_ord_default

#' @rdname methods-ade4
#' @export
recover_rows.nipals <- function(x) {
  res <- x[["li"]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-ade4
#' @export
recover_cols.nipals <- function(x) {
  res <- x[["c1"]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-ade4
#' @export
recover_inertia.nipals <- function(x) {
  x[["eig"]]
}

#' @rdname methods-ade4
#' @export
recover_coord.nipals <- function(x) {
  paste0("Fac", seq(x$nf))
}

#' @rdname methods-ade4
#' @export
recover_conference.nipals <- function(x) {
  # `ade4::nipals()` normalizes the column coordinates
  c(1, 0)
}

#' @rdname methods-ade4
#' @export
augmentation_rows.nipals <- function(x) {
  .name <- rownames(x[["li"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["li"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-ade4
#' @export
augmentation_cols.nipals <- function(x) {
  .name <- rownames(x[["c1"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["c1"]]))
  } else {
    tibble(.name = .name)
  }
  if (! is.null(attr(x, "cmeans"))) {
    res <- dplyr::bind_cols(res, .cmeans = attr(x, "cmeans"))
  }
  if (! is.null(attr(x, "csd"))) {
    res <- dplyr::bind_cols(res, .csd = attr(x, "csd"))
  }
  res
}

#' @rdname methods-ade4
#' @export
augmentation_coord.nipals <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .eig = x[["eig"]],
    .nb = x[["nb"]]
  )
}
