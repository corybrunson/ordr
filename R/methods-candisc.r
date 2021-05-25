#' @title Functionality for canonical discriminant ('candisc') and correlation
#'   ('cancor') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class 'candisc' and 'cancor' from the
#'   **[candisc][candisc::candisc-package]** package.
#'
#' @name methods-candisc
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-candisc-.r
NULL

#' @rdname methods-candisc
#' @export
as_tbl_ord.cancor <- as_tbl_ord_default

#' @rdname methods-candisc
#' @export
recover_rows.cancor <- function(x) {
  res <- cov(x$X) %*% x$coef$X
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-candisc
#' @export
recover_cols.cancor <- function(x) {
  res <- cov(x$Y) %*% x$coef$Y
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-candisc
#' @export
recover_inertia.cancor <- function(x) x$cancor^2

#' @rdname methods-candisc
#' @export
recover_coord.cancor <- function(x) paste0("can", seq_along(x$cancor))

#' @rdname methods-candisc
#' @export
recover_conference.cancor <- function(x) {
  # `candisc::cancor()` returns canonical weights, i.e. standard coefficients
  c(0, 0)
}

#' @rdname methods-candisc
#' @export
augmentation_rows.cancor <- function(x) {
  .name <- x$names$X
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$coef$X))
  } else {
    tibble(.name = .name)
  }
  # case scores as supplementary points
  res_sup <- if (is.null(x$names$row.names)) {
    tibble_pole(nrow(x$scores$X))
  } else {
    tibble(.name = x$names$row.names)
  }
  res_sup$.weight <- x$weights
  # supplement flag
  res$.supplement <- FALSE
  res_sup$.supplement <- TRUE
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-candisc
#' @export
augmentation_cols.cancor <- function(x) {
  .name <- x$names$Y
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$coef$Y))
  } else {
    tibble(.name = .name)
  }
  # case scores as supplementary points
  res_sup <- if (is.null(x$names$row.names)) {
    tibble_pole(nrow(x$scores$Y))
  } else {
    tibble(.name = x$names$row.names)
  }
  res_sup$.weight <- x$weights
  # supplement flag
  res$.supplement <- FALSE
  res_sup$.supplement <- TRUE
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-candisc
#' @export
augmentation_coord.cancor <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .cancor = x$cancor
  )
}

#' @rdname methods-candisc
#' @export
supplementation_rows.cancor <- function(x) x$scores$X

#' @rdname methods-candisc
#' @export
supplementation_cols.cancor <- function(x) x$scores$Y
