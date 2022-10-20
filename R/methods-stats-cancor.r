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
#' If canonical scores are returned, then they and the structure correlations
#' are made available as supplementary elements. **ordr** takes rows and columns
#' from the intraset correlations `$xstructure` and `$ystructure`, on which no
#' intertia is conferred; the interset correlations can be obtained by
#' [conferring inertia][conference] onto these.
#'
#' A biplot of the canonical coefficients can be interpreted as approximating
#' the \eqn{X}-\eqn{Y} inner product matrix, inversely weighted by the \eqn{X}
#' and \eqn{Y} variances. The canonical scores and structure coefficients are
#' available as supplementary points if returned by [cancor_ord()]. These can be
#' used to create biplots of the case scores as linear combinations of loadings
#' (the coefficients, in standard coordinates, overlaid with the scores) or of
#' intraset and interset correlations with respect to either data set (the
#' correlations with inertia conferred entirely onto rows or onto columns).
#' Greenacre (1984) and ter Braak (1990) describe these families, though ter
#' Braak recommends against the first.
#'
#' @template ref-greenacre1984
#' @template ref-braak1990
#'
#' @name methods-cancor
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for singular value decomposition-based techniques
#' @family models from the stats package
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
recover_supp_rows.cancor_ord <- function(x) {
  rbind(x$xscores, x$xstructure)[, seq_along(x$cor), drop = FALSE]
}

#' @rdname methods-cancor
#' @export
recover_supp_cols.cancor_ord <- function(x) {
  rbind(x$yscores, x$ystructure)[, seq_along(x$cor), drop = FALSE]
}

#' @rdname methods-cancor
#' @export
recover_aug_rows.cancor_ord <- function(x) {
  name <- rownames(x$xcoef)
  res <- if (is.null(name)) {
    tibble_pole(nrow(x$xcoef))
  } else {
    tibble(name = name)
  }
  res$center <- unname(x$xcenter)
  # case scores and structure correlations as supplementary points
  res_sup <- NULL
  if (! is.null(x$xscores)) {
    res_sup_elt <- if (is.null(rownames(x$xscores))) {
      tibble_pole(nrow(x$xscores))
    } else {
      tibble(name = rownames(x$xscores))
    }
    res_sup_elt$.element <- "score"
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  if (! is.null(x$xstructure)) {
    res_sup_elt <- if (is.null(rownames(x$xstructure))) {
      tibble_pole(nrow(x$xstructure))
    } else {
      tibble(name = rownames(x$xstructure))
    }
    res_sup_elt$.element <- "structure"
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  # supplement flag
  res$.element <- "active"
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-cancor
#' @export
recover_aug_cols.cancor_ord <- function(x) {
  name <- rownames(x$ycoef)
  res <- if (is.null(name)) {
    tibble_pole(nrow(x$ycoef))
  } else {
    tibble(name = name)
  }
  res$center <- unname(x$ycenter)
  # case scores and structure correlations as supplementary points
  res_sup <- NULL
  if (! is.null(x$xscores)) {
    res_sup_elt <- if (is.null(rownames(x$yscores))) {
      tibble_pole(nrow(x$yscores))
    } else {
      tibble(name = rownames(x$yscores))
    }
    res_sup_elt$.element <- "score"
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  if (! is.null(x$ystructure)) {
    res_sup_elt <- if (is.null(rownames(x$ystructure))) {
      tibble_pole(nrow(x$ystructure))
    } else {
      tibble(name = rownames(x$ystructure))
    }
    res_sup_elt$.element <- "structure"
    res_sup <- bind_rows(res_sup, res_sup_elt)
  }
  # supplement flag
  res$.element <- "active"
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-cancor
#' @export
recover_aug_coord.cancor_ord <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    cor = x$cor
  )
}
