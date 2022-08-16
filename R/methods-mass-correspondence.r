#' @title Functionality for correspondence analysis ('correspondence') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"correspondence"` from the **[MASS][MASS::corresp]**
#'   package.
#'
#' @name methods-correspondence
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for singular value decomposition-based techniques
#' @example inst/examples/ex-methods-corresp-quine.r
NULL

#' @rdname methods-correspondence
#' @export
as_tbl_ord.correspondence <- as_tbl_ord_default

#' @rdname methods-correspondence
#' @export
recover_rows.correspondence <- function(x) {
  res <- as.matrix(x$rscore)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-correspondence
#' @export
recover_cols.correspondence <- function(x) {
  res <- as.matrix(x$cscore)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-correspondence
#' @export
recover_inertia.correspondence <- function(x) x$cor ^ 2

#' @rdname methods-correspondence
#' @export
recover_conference.correspondence <- function(x) {
  # `MASS::corresp()` returns row and column standard coordinates
  c(0, 0)
}

#' @rdname methods-correspondence
#' @export
recover_coord.correspondence <- function(x) {
  paste0("Can", seq(ncol(as.matrix(x$rscore))))
}

#' @rdname methods-correspondence
#' @export
recover_aug_rows.correspondence <- function(x) {
  name <- rownames(as.matrix(x$rscore))
  if (is.null(name)) {
    tibble_pole(nrow(as.matrix(x$rscore)))
  } else {
    tibble(name = name)
  }
}

#' @rdname methods-correspondence
#' @export
recover_aug_cols.correspondence <- function(x) {
  name <- rownames(as.matrix(x$cscore))
  if (is.null(name)) {
    tibble_pole(nrow(as.matrix(x$cscore)))
  } else {
    tibble(name = name)
  }
}

#' @rdname methods-correspondence
#' @export
recover_aug_coord.correspondence <- function(x){
  tibble(
    name = factor_coord(recover_coord(x)),
    cor = x$cor
  )
}
