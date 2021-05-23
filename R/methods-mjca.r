#' @title Functionality for multiple and joint correspondence analysis ('mjca')
#'   objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"mjca"` from the **[ca][ca::mjca]** package.
#'
#' @name methods-mjca
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-mjca-admissions.r
NULL

#' @rdname methods-mjca
#' @export
as_tbl_ord.mjca <- as_tbl_ord_default

#' @rdname methods-mjca
#' @export
recover_rows.mjca <- function(x) {
  res <- x$rowcoord
  rownames(res) <- x$rownames
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-mjca
#' @export
recover_cols.mjca <- function(x) {
  res <- x$colcoord
  rownames(res) <- x$levelnames
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-mjca
#' @export
recover_inertia.mjca <- function(x) x$sv[seq(x$nd)] ^ 2

#' @rdname methods-mjca
#' @export
recover_conference.mjca <- function(x) {
  # `ca::mjca()` always returns row and column standard coordinates
  c(0, 0)
}

#' @rdname methods-mjca
#' @export
recover_coord.mjca <- function(x) paste0("Dim", seq(ncol(x$rowcoord)))

#' @rdname methods-mjca
#' @export
augmentation_rows.mjca <- function(x) {
  .name <- x$rownames
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$rowcoord))
  } else {
    tibble(.name = .name)
  }
  dplyr::bind_cols(
    res,
    .mass = x$rowmass,
    .dist = x$rowdist,
    .inertia = x$rowinertia
  )
}

#' @rdname methods-mjca
#' @export
augmentation_cols.mjca <- function(x){
  .name <- x$levelnames
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$colcoord))
  } else {
    tibble(.name = .name)
  }
  dplyr::bind_cols(
    res,
    `colnames<-`(x$factors, paste0(".", colnames(x$factors))),
    .mass = x$colmass,
    .dist = x$coldist,
    .inertia = x$colinertia
  )
}

#' @rdname methods-mjca
#' @export
augmentation_coord.mjca <- function(x){
  tibble(
    .name = factor_coord(recover_coord(x)),
    .sv = x$sv[seq(x$nd)]
  )
}
