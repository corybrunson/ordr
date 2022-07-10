#' @title Functionality for factor analysis ('factanal') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"factanal"` as returned by [stats::factanal()].
#'
#' @details
#'
#' Most **ordr** methods will only work if the 'factanal' object includes a
#' `$scores` matrix, hence if a data matrix is provided.
#'
#' **NB:** `factanal()` normalizes the columns of `x` to unit length before it
#' calculates their loadings, so, for consistency with the behavior of other
#' methods (e.g. those of [lda_ord()]), inertia is assumed to lie with the rows.
#' However, the scores (retrieved by `recover_rows()`) are not factors of a
#' matrix decomposition and are better understood as predictions like those of
#' `lda_ord()`, so redistributing inertia to the loadings should be done
#' cautiously.
#' 

#' @name methods-factanal
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-factanal-swiss.r
NULL

#' @rdname methods-factanal
#' @export
as_tbl_ord.factanal <- as_tbl_ord_default

#' @rdname methods-factanal
#' @export
recover_rows.factanal <- function(x) {
  x[["scores"]]
}

#' @rdname methods-factanal
#' @export
recover_cols.factanal <- function(x) {
  unclass(x[["loadings"]])
}

#' @rdname methods-factanal
#' @export
recover_inertia.factanal <- function(x) {
  colSums(x[["loadings"]] ^ 2)
}

#' @rdname methods-factanal
#' @export
recover_coord.factanal <- function(x) {
  colnames(x[["loadings"]])
}

#' @rdname methods-factanal
#' @export
recover_conference.factanal <- function(x) {
  # columns are scaled to unit lengths before loadings are calculated
  c(1, 0)
}

#' @rdname methods-factanal
#' @export
augmentation_rows.factanal <- function(x) {
  .name <- rownames(x[["scores"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["scores"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-factanal
#' @export
augmentation_cols.factanal <- function(x) {
  .name <- rownames(x[["loadings"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["loadings"]]))
  } else {
    tibble(.name = .name)
  }
  res$.uniqueness <- x$uniquenesses
  res
}

#' @rdname methods-factanal
#' @export
augmentation_coord.factanal <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x))
  )
}
