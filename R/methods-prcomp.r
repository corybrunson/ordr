#' Functionality for principal components analysis ('prcomp') objects
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"prcomp"}.
#' 
#' @name methods-prcomp
#' @template param-methods
#' @example inst/examples/ex-prcomp.r

#' @rdname methods-prcomp
#' @export
as_tbl_ord.prcomp <- as_tbl_ord_default

#' @rdname methods-prcomp
#' @export
reconstruct.prcomp <- function(x) {
  res <- recover_u.prcomp(x)%*%t(recover_v.prcomp(x))
  if (x[["center"]] == FALSE && x[["scale"]] == FALSE) {
    res
  } else if (x[["center"]] != TRUE && x[["scale"]] == FALSE) {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- res[row, col] + x[["center"]][col]}}
    res
  } else {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]}}
    res
  }
}

#' @rdname methods-prcomp
#' @export
recover_u.prcomp <- function(x) {
  x[["x"]]
}

#' @rdname methods-prcomp
#' @export
recover_v.prcomp <- function(x) {
  x[["rotation"]]
}

#' @rdname methods-prcomp
#' @export
recover_inertia.prcomp <- function(x) {
  (x[["sdev"]] ^ 2) * (nrow(x[["x"]]) - 1)
}

#' @rdname methods-prcomp
#' @export
recover_coord.prcomp <- function(x) {
  colnames(x[["rotation"]])
}

#' @rdname methods-prcomp
#' @export
recover_conference.prcomp <- function(x) {
  # `stats::prcomp()` returns the rotated data
  c(1, 0)
}

#' @rdname methods-prcomp
#' @export
augment_u.prcomp <- function(x) {
  tibble(
    .name = rownames(x[["x"]])
  )
}

#' @rdname methods-prcomp
#' @export
augment_v.prcomp <- function(x) {
  res <- tibble(.name = rownames(x[["rotation"]]))
  if (class(x[["center"]]) == "numeric") {
    res <- dplyr::bind_cols(res, .center = x[["center"]])
  }
  if (class(x[["scale"]]) == "numeric") {
    res <- dplyr::bind_cols(res, .scale = x[["scale"]])
  }
  res
}

#' @rdname methods-prcomp
#' @export
augment_coord.prcomp <- function(x) {
  tibble(
    .name = recover_coord(x),
    .sdev = x[["sdev"]]
  )
}
