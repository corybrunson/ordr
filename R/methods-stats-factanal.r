#' @title Functionality for factor analysis ('factanal') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"factanal"` as returned by [stats::factanal()].
#'
#' @details
#'
#' Factor analysis of a data matrix relies on an an eigendecomposition of its
#' correlation matrix, whose eigenvectors (up to weighting) comprise the
#' variable loadings. For this reason, both row and column recoverers retrieve
#' the loadings and inertia is evenly distributed between them. When computed
#' and returned by [stats::factanal()], the case scores are accessible as
#' supplementary elements. Redistribution of inertia commutes through both
#' score calculations.
#' 

#' @name methods-factanal
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for eigen-decomposition-based techniques
#' @family models from the stats package
#' @example inst/examples/ex-methods-factanal-swiss.r
NULL

#' @rdname methods-factanal
#' @export
as_tbl_ord.factanal <- as_tbl_ord_default

recover_dims_factanal <- function(x, .matrix) unclass(x[["loadings"]])

#' @rdname methods-factanal
#' @export
recover_rows.factanal <- function(x) recover_dims_factanal(x, "rows")

#' @rdname methods-factanal
#' @export
recover_cols.factanal <- function(x) recover_dims_factanal(x, "cols")

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
recover_supp_rows.factanal <- function(x) {
  x[["scores"]]
}

#' @rdname methods-factanal
#' @export
recover_aug_rows.factanal <- function(x) {
  name <- rownames(x[["loadings"]])
  res <- if (is.null(name)) {
    tibble(.rows = nrow(x[["loadings"]]))
  } else {
    tibble(name = name)
  }
  if (is.null(x[["scores"]])) return(res)
  
  # factor scores as supplementary points
  res_sup <- if (is.null(rownames(x[["scores"]]))) {
    tibble(.rows = x[["n.obs"]])
  } else {
    tibble(name = rownames(x[["scores"]]))
  }
  # supplement flag
  res$.element <- "active"
  res_sup$.element <- "score"
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-factanal
#' @export
recover_aug_cols.factanal <- function(x) {
  name <- rownames(x[["loadings"]])
  res <- if (is.null(name)) {
    tibble(.rows = nrow(x[["loadings"]]))
  } else {
    tibble(name = name)
  }
  res$uniqueness <- x$uniquenesses
  res$.element <- "active"
  res
}

#' @rdname methods-factanal
#' @export
recover_aug_coord.factanal <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x))
  )
}
