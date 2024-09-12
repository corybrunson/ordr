#' @title Functionality for principal components analysis ('princomp') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"princomp"` as returned by [stats::princomp()].
#'   
#' @details
#' 
#' Principal components analysis (PCA), as performed by [stats::princomp()],
#' relies on an eigenvalue decomposition (EVD) of the covariance matrix 
#' \eqn{X^TX} of a data set \eqn{X}. [stats::princomp()] returns the EVD factor
#' \eqn{V} as the loadings `$loadings`. The scores `$scores` are obtained as
#' \eqn{XV} and are accessible as supplementary elements.
#'
#' @name methods-princomp
#' @author Emily Paul, John Gracey
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for eigendecomposition-based techniques
#' @family models from the stats package
#' @example inst/examples/ex-methods-princomp-iris.r
NULL

#' @rdname methods-princomp
#' @export
as_tbl_ord.princomp <- as_tbl_ord_default

#' @rdname methods-princomp
#' @export
recover_rows.princomp <- function(x) {
  matrix(nrow = 0, ncol = ncol(x[["loadings"]]),
         dimnames = list(NULL, colnames(x[["loadings"]])))
}

#' @rdname methods-princomp
#' @export
recover_cols.princomp <- function(x) {
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
recover_supp_rows.princomp <- function(x) {
  x[["scores"]]
}

#' @rdname methods-princomp
#' @export
recover_aug_rows.princomp <- function(x) {
  name <- rownames(x$loadings)
  res <- if (is.null(name)) {
    matrix(nrow = nrow(x$loadings), ncol = 0)
  } else {
    tibble(name = name)
  }
  
  # scores as supplementary points
  name <- rownames(x$scores)
  res_sup <- if (is.null(name)) {
    matrix(nrow = nrow(x$loadings), ncol = 0)
  } else {
    tibble(name = name)
  }
  
  # supplement flag
  res$.element <- "active"
  res_sup$.element <- "score"
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-princomp
#' @export
recover_aug_cols.princomp <- function(x) {
  name <- rownames(x[["loadings"]])
  res <- if (is.null(name)) {
    tibble_pole(nrow(x[["loadings"]]))
  } else {
    tibble(name = name)
  }
  bind_cols(res, tibble(
    center = x[["center"]],
    scale = x[["scale"]]
  ))
}

#' @rdname methods-princomp
#' @export
recover_aug_coord.princomp <- function(x) {
  tibble(
    name = factor_coord(recover_coord.princomp(x)),
    sdev = x[["sdev"]]
  )
}
