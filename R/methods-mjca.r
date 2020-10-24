#' @title Functionality for multiple and joint correspondence analysis ('mjca')
#'   objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"mjca"` from the **[ca][ca::mjca]** package.
#'
#' @name methods-mjca
#' @include ord-tbl.r
#' @template param-methods
NULL

#' @rdname methods-mjca
#' @export
as_tbl_ord.mjca <- as_tbl_ord_default

#' @rdname methods-mjca
#' @export
recover_u.mjca <- function(x) x$rowcoord

#' @rdname methods-mjca
#' @export
recover_v.mjca <- function(x) x$colcoord

#' @rdname methods-mjca
#' @export
recover_inertia.mjca <- function(x) x$sv ^ 2

#' @rdname methods-mjca
#' @export
recover_conference.mjca <- function(x) {
  # `ca::ca()` always returns row and column standard coordinates
  c(0, 0)
}

#' @rdname methods-mjca
#' @export
recover_coord.mjca <- function(x) {
  colnames(x$rowcoord)
}

#' @rdname methods-mjca
#' @export
augmentation_u.mjca <- function(x) {
  .name <- rownames(x$rowcoord)
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
augmentation_v.mjca <- function(x){
  .name <- rownames(x$colcoord)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$colcoord))
  } else {
    tibble(.name = .name)
  }
  dplyr::bind_cols(
    res,
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
    .sv = x$sv
  )
}
