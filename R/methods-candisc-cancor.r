#' @title Functionality for canonical correlation ('cancor') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class 'cancor' from the **[candisc][candisc::candisc-package]**
#'   package.
#'
#' @details
#'
#' ter Braak (1990) recommends two families of biplots for the canonical
#' correspondence analysis of data matrices \eqn{X} and \eqn{Y}. For the first,
#' using structural correlations, either the interset correlations of \eqn{X}
#' (equivalent to principal coordinates) are plotted with the intraset
#' correlations of \eqn{Y} (standard coordinates) or vice-versa. Where `cc` is
#' the output of [candisc::cancor()], these are obtained from `cc$structure`.
#' For consistency with the canonical variate scores `cc$scores` available as
#' supplementary points, **ordr** takes rows and columns from the intraset
#' correlations `cc$structure$X.xscores` and `cc$structure$Y.yscores`, so that
#' no intertia conferred.
#' 

#' @template ref-braak1990
#'   

#' @name methods-cancor
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-cancor-savings.r
NULL

#' @rdname methods-cancor
#' @export
as_tbl_ord.cancor <- as_tbl_ord_default

#' @rdname methods-cancor
#' @export
recover_rows.cancor <- function(x) {
  res <- x$structure$X.xscores
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_cols.cancor <- function(x) {
  res <- x$structure$Y.yscores
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_inertia.cancor <- function(x) x$cancor^2

#' @rdname methods-cancor
#' @export
recover_coord.cancor <- function(x) paste0("can", seq_along(x$cancor))

#' @rdname methods-cancor
#' @export
recover_conference.cancor <- function(x) {
  # `x$structure$*` are structure correlations; rows grab intraset ones
  c(0, 0)
}

#' @rdname methods-cancor
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

#' @rdname methods-cancor
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

#' @rdname methods-cancor
#' @export
augmentation_coord.cancor <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .cancor = x$cancor
  )
}

#' @rdname methods-cancor
#' @export
supplementation_rows.cancor <- function(x) x$scores$X

#' @rdname methods-cancor
#' @export
supplementation_cols.cancor <- function(x) x$scores$Y
