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
  unclass(x[["loadings"]])
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
  # loadings are assigned half the diagonal from the eigendecomposition
  c(.5, .5)
}

#' @rdname methods-factanal
#' @export
supplementation_rows.factanal <- function(x) {
  x[["scores"]]
}

#' @rdname methods-factanal
#' @export
augmentation_rows.factanal <- function(x) {
  .name <- rownames(x[["loadings"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["loadings"]]))
  } else {
    tibble(.name = .name)
  }
  if (is.null(x[["scores"]])) return(res)
  
  # factor scores as supplementary points
  res_sup <- if (is.null(rownames(x[["scores"]]))) {
    tibble_pole(x[["n.obs"]])
  } else {
    tibble(.name = rownames(x[["scores"]]))
  }
  # supplement flag
  res$.supplement <- FALSE
  res_sup$.supplement <- TRUE
  as_tibble(dplyr::bind_rows(res, res_sup))
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
