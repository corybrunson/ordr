#' @title Functionality for multiple correspondence analysis ('mca') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"mca"` from the **[MASS][MASS::mca]** package.
#'
#' @details
#'
#' Multiple correspondence analysis (MCA) relies on a singular value
#' decomposition of the indicator matrix \eqn{X} of a table of several
#' categorical variables, scaled by its column totals. [MASS::mca()] returns the
#' SVD factors \eqn{UD} and \eqn{V} as the row weights `$fs`, on which the
#' inertia is conferred, and the column coordinates `$cs`. The row coordinates
#' `$rs` are obtained as \eqn{XV} and accessible as supplementary elements.
#'
#' @name methods-mca
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for singular value decomposition-based techniques
#' @example inst/examples/ex-methods-mca-admissions.r
NULL

#' @rdname methods-mca
#' @export
as_tbl_ord.mca <- as_tbl_ord_default

#' @rdname methods-mca
#' @export
recover_rows.mca <- function(x) {
  `colnames<-`(x$fs, recover_coord(x))
}

#' @rdname methods-mca
#' @export
recover_cols.mca <- function(x) {
  `colnames<-`(x$cs, recover_coord(x))
}

#' @rdname methods-mca
#' @export
recover_inertia.mca <- function(x) x$d ^ 2

#' @rdname methods-mca
#' @export
recover_conference.mca <- function(x) {
  # `MASS::mca()` returns row weights from u and d & column coordinates from v
  c(1, 0)
}

#' @rdname methods-mca
#' @export
recover_coord.mca <- function(x) paste0("Dim", seq_along(x$d))

#' @rdname methods-mca
#' @export
recover_supp_rows.mca <- function(x) {
  `colnames<-`(x$rs, recover_coord(x))
}

#' @rdname methods-mca
#' @export
recover_aug_rows.mca <- function(x) {
  name <- rownames(x$fs)
  res <- if (is.null(name)) {
    tibble_pole(nrow(x$fs))
  } else {
    tibble(name = name)
  }
  
  # row coordinates as supplementary points
  name <- rownames(x$rs)
  res_sup <- if (is.null(name)) {
    tibble_pole(nrow(x$rs))
  } else {
    tibble(name = name)
  }
  
  # supplement flag
  res$.element <- "active"
  res_sup$.element <- "score"
  as_tibble(dplyr::bind_rows(res, res_sup))
}

#' @rdname methods-mca
#' @export
recover_aug_cols.mca <- function(x) {
  name <- rownames(x$cs)
  # introduce `.factor` and `.level` according to `abbrev`
  if (is.null(name)) {
    tibble_pole(nrow(x$cs))
  } else if (is.null(attr(rownames(x$cs), "names"))) {
    # only add `.factor` and `.level` if names are unambiguous
    level_ambig <- any(grepl("\\..*\\.", rownames(x$cs)))
    tibble(
      name = name,
      factor = if (! level_ambig) gsub("\\..*$", "", name),
      level = if (! level_ambig) gsub("^.*\\.", "", name),
      .element = "active"
    )
  } else {
    tibble(
      name = names(rownames(x$cs)),
      level = unname(rownames(x$cs))
    )
  }
}

#' @rdname methods-mca
#' @export
recover_aug_coord.mca <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    sv = x$d
  )
}
