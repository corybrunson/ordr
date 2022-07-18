#' @title Functionality for canonical correlation ('cancor') and discriminant
#'   ('candisc') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class 'cancor' and 'candisc' from the
#'   **[candisc][candisc::candisc-package]** package.
#'
#' @details
#'
#' ter Braak (1990) recommends two families of biplots for the canonical
#' correspondence analysis of data matrices \eqn{X} and \eqn{Y}. For the first,
#' using structural correlations, either the interset correlations of \eqn{X}
#' (equivalent to principal coordinates) are plotted with the intraset
#' correlations of \eqn{Y} (standard coordinates) or vice-versa, so that their
#' product recovers the inner product matrix \eqn{XY'}. Where `cc` is the output
#' of [candisc::cancor()] on centered and scaled data matrices, these are
#' obtained from `cc$structure`. For consistency with the canonical variate
#' scores `cc$scores` available as supplementary points, **ordr** takes rows and
#' columns from the intraset correlations `cc$structure$X.xscores` and
#' `cc$structure$Y.yscores`, on which no intertia is conferred. ter Braak's
#' biplots can then be recovered by [balancing][conference] the inertia across
#' the two factors.
#' 

#' @template ref-braak1990
#'   

#' @name methods-candisc
#' @include ord-tbl.r
#' @template param-methods
#' @family methods for singular value decomposition-based techniques
#' @example inst/examples/ex-methods-candisc-savings.r
NULL

#' @rdname methods-candisc
#' @export
as_tbl_ord.cancor <- as_tbl_ord_default

#' @rdname methods-candisc
#' @export
recover_rows.cancor <- function(x) {
  res <- x$structure$X.xscores
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-candisc
#' @export
recover_cols.cancor <- function(x) {
  res <- x$structure$Y.yscores
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
  # `x$structure$*` are structure correlations; rows grab intraset ones
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
