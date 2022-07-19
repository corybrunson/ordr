#' @title Functionality for canonical correlations
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"cancor_ord"`. This is a class introduced in this package
#'   to identify objects returned by [cancor_ord()], which wraps
#'   [stats::cancor()].
#'
#' @details
#'
#' The canonical coefficients (loadings) are obtained directly from the
#' underlying singular value decomposition and constitute the active elements.
#' If scores are returned, then they are made available as supplementary
#' elements.
#'
#' A biplot of the canonical coefficients can be interpreted as approximating
#' the \eqn{X}-\eqn{Y} covariance matrix, inversely weighted by the \eqn{X} and
#' \eqn{Y} variances. A biplot of the canonical scores of one matrix, available
#' as supplementary points if returned by [cancor_ord()], and the coefficients
#' of the other can be interpreted as .... Both biplots can have inertia
#' conferred to either or both factors. Greenacre (1984) describes both biplots,
#' though ter Braak (1990) recommends against the former.
#'
#' @template ref-greenacre1984
#' @template ref-braak1990
#'
#' @name methods-cancor
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-cancor-savings.r
NULL

#' @rdname methods-cancor
#' @export
as_tbl_ord.cancor_ord <- as_tbl_ord_default

#' @rdname methods-cancor
#' @export
recover_rows.cancor_ord <- function(x) {
  res <- x$xcoef[, seq_along(x$cor), drop = FALSE]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_cols.cancor_ord <- function(x) {
  res <- x$ycoef[, seq_along(x$cor), drop = FALSE]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_inertia.cancor_ord <- function(x) x$cor^2

#' @rdname methods-cancor
#' @export
recover_coord.cancor_ord <- function(x) paste0("CanCor", seq_along(x$cor))

#' @rdname methods-cancor
#' @export
recover_conference.cancor_ord <- function(x) {
  # `stats::cancor()` returns canonical weights, i.e. standard coefficients
  c(0, 0)
}

#' @rdname methods-cancor
#' @export
augmentation_rows.cancor_ord <- function(x) {
  .name <- rownames(x$xcoef)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$xcoef))
  } else {
    tibble(.name = .name)
  }
  res$.center <- unname(x$xcenter)
  # case scores and structure correlations as supplementary points
  res_sup <- NULL
  if (! is.null(x$xscores)) {
    res_sup_elt <- if (is.null(rownames(x$xscores))) {
      tibble_pole(nrow(x$xscores))
    } else {
      tibble(.name = rownames(x$xscores))
    }
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  if (! is.null(x$x.xscores)) {
    res_sup_elt <- if (is.null(rownames(x$x.xscores))) {
      tibble_pole(nrow(x$x.xscores) + nrow(x$y.xscores))
    } else {
      tibble(.name = c(rownames(x$x.xscores), rownames(x$y.xscores)))
    }
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  # supplement flag
  res$.supplement <- FALSE
  if (! is.null(res_sup)) res_sup$.supplement <- TRUE
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-cancor
#' @export
augmentation_cols.cancor_ord <- function(x) {
  .name <- rownames(x$ycoef)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$ycoef))
  } else {
    tibble(.name = .name)
  }
  res$.center <- unname(x$ycenter)
  # case scores and structure correlations as supplementary points
  res_sup <- NULL
  if (! is.null(x$xscores)) {
    res_sup_elt <- if (is.null(rownames(x$yscores))) {
      tibble_pole(nrow(x$yscores))
    } else {
      tibble(.name = rownames(x$yscores))
    }
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  if (! is.null(x$x.yscores)) {
    res_sup_elt <- if (is.null(rownames(x$x.yscores))) {
      tibble_pole(nrow(x$x.yscores) + nrow(x$y.yscores))
    } else {
      tibble(.name = c(rownames(x$x.yscores), rownames(x$y.yscores)))
    }
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  # supplement flag
  res$.supplement <- FALSE
  if (! is.null(res_sup)) res_sup$.supplement <- TRUE
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-cancor
#' @export
augmentation_coord.cancor_ord <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .cor = x$cor
  )
}

#' @rdname methods-cancor
#' @export
supplementation_rows.cancor_ord <- function(x) {
  rbind(x$xscores, x$x.xscores, x$y.xscores)[, seq_along(x$cor), drop = FALSE]
}

#' @rdname methods-cancor
#' @export
supplementation_cols.cancor_ord <- function(x) {
  rbind(x$yscores, x$x.yscores, x$y.yscores)[, seq_along(x$cor), drop = FALSE]
}
