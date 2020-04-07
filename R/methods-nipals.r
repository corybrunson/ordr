#' @title Functionality for non-linear iterative PLS ('nipals') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"nipals"` as returned by [ade4::nipals()].
#'
#' @name methods-nipals
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/country-prcomp-confer.r
#' @example inst/examples/iris-prcomp-center-ellipse.r
#' @example inst/examples/iris-prcomp-lineranges.r
NULL

#' @rdname methods-nipals
#' @export
as_tbl_ord.nipals <- as_tbl_ord_default

#' @rdname methods-nipals
#' @export
reconstruct.nipals <- function(x) {
  res <- recover_u.nipals(x) %*% t(recover_v.nipals(x))
  if (! is.null(attr(x, "cmeans")) & ! is.null(attr(x, "csd"))) {
    res <- sweep(sweep(res, 2, attr(x, "cmeans"), "*"),
                 2, attr(x, "cmeans"), "+")
    return(res)
  } else {
    warning("Scaling factors `cmeans` and `csd` were not recovered.")
    return(res)
  }
}

#' @rdname methods-nipals
#' @export
recover_u.nipals <- function(x) {
  res <- x[["li"]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-nipals
#' @export
recover_v.nipals <- function(x) {
  res <- x[["c1"]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-nipals
#' @export
recover_inertia.nipals <- function(x) {
  x[["eig"]]
}

#' @rdname methods-nipals
#' @export
recover_coord.nipals <- function(x) {
  paste0("Fac", seq(x$nf))
}

#' @rdname methods-nipals
#' @export
recover_conference.nipals <- function(x) {
  # `ade4::nipals()` normalizes the column coordinates
  c(1, 0)
}

#' @rdname methods-nipals
#' @export
augmentation_u.nipals <- function(x) {
  .name <- rownames(x[["li"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["li"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-nipals
#' @export
augmentation_v.nipals <- function(x) {
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

#' @rdname methods-nipals
#' @export
augmentation_coord.nipals <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .eig = x[["eig"]],
    .nb = x[["nb"]]
  )
}
