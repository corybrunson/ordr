#' @title Functionality for k-means clustering ('kmeans') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"kmeans"` as returned by [stats::kmeans()].
#'
#' @name methods-kmeans
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/mtcars-kmeans-augment.r
NULL

#' @rdname methods-kmeans
#' @export
as_tbl_ord.kmeans <- as_tbl_ord_default

#' @rdname methods-kmeans
#' @export
reconstruct.kmeans <- function(x) {
  x$centers[x$cluster, , drop = FALSE]
}

#' @rdname methods-kmeans
#' @export
recover_u.kmeans <- function(x) {
  res <- outer(x$cluster, 1:length(x$size), "==")
  mode(res) <- "integer"
  colnames(res) <- rownames(x$centers)
  res
}

#' @rdname methods-kmeans
#' @export
recover_v.kmeans <- function(x) {
  t(x$centers)
}

#' @rdname methods-kmeans
#' @export
recover_coord.kmeans <- function(x) {
  rownames(x$centers)
}

#' @rdname methods-kmeans
#' @export
augmentation_u.kmeans <- function(x) {
  .name <- names(x$cluster)
  res <- if (is.null(.name)) {
    tibble_pole(length(x$cluster))
  } else {
    tibble(.name = .name)
  }
  res$.cluster <- factor(unname(x$cluster))
  res
}

#' @rdname methods-kmeans
#' @export
augmentation_v.kmeans <- function(x) {
  .name <- colnames(x$centers)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$centers))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-kmeans
#' @export
augmentation_coord.kmeans <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .size = x$size,
    .withinss = x$withinss
  )
}
