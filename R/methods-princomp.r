#' @title Functionality for principal components analysis ('princomp') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"princomp"` as returned by [stats::princomp()].
#'
#' @name methods-princomp
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/iris-princomp-center-star.r
#' @example inst/examples/iris-princomp-sec.r
NULL

#' @rdname methods-princomp
#' @export
as_tbl_ord.princomp <- as_tbl_ord_default

#' @rdname methods-princomp
#' @export
reconstruct.princomp <- function(x) {
  res <- x[["scores"]] %*% t(x[["loadings"]])
  for (col in 1:ncol(res)) {
    for (row in 1:nrow(res)) {
      res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]
    }
  }
  res
}

#' @rdname methods-princomp
#' @export
recover_u.princomp <- function(x) {
  x[["scores"]]
}

#' @rdname methods-princomp
#' @export
recover_v.princomp <- function(x) {
  unclass(x[["loadings"]])
}

#' @rdname methods-princomp
#' @export
recover_inertia.princomp <- function(x) {
  (x[["sdev"]] ^ 2) * x[["n.obs"]]
}

#' @rdname methods-princomp
#' @export
recover_coord.princomp <- function(x) {
  colnames(x[["scores"]])
}

#' @rdname methods-princomp
#' @export
recover_conference.princomp <- function(x) {
  # `stats::princomp()` returns the rotated data
  c(1, 0)
}

#' @rdname methods-princomp
#' @export
augmentation_u.princomp <- function(x) {
  .name <- rownames(x[["scores"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["scores"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-princomp
#' @export
augmentation_v.princomp <- function(x) {
  .name <- rownames(x[["loadings"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["loadings"]]))
  } else {
    tibble(.name = .name)
  }
  bind_cols(res, tibble(
    .center = x[["center"]],
    .scale = x[["scale"]]
  ))
}

#' @rdname methods-princomp
#' @export
augmentation_coord.princomp <- function(x) {
  tibble(
    .name = recover_coord.princomp(x),
    .sdev = x[["sdev"]]
  )
}
