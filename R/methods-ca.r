#' @title Functionality for correspondence analysis ('ca') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"ca"` from the **[ca][ca::ca]** package.
#'
#' @name methods-ca
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/finches-ca.r
#' @example inst/examples/smoke-ca-confer.r
#' @example inst/examples/benthos-ca-augment-confer.r
#' @example inst/examples/women-ca-confer.r
#' @example inst/examples/haireye-ca-rotate.r
#' @example inst/examples/ratios-ca-verbs.r
NULL

#' @rdname methods-ca
#' @export
as_tbl_ord.ca <- as_tbl_ord_default

#' @rdname methods-ca
#' @export
recover_u.ca <- function(x) x$rowcoord

#' @rdname methods-ca
#' @export
recover_v.ca <- function(x) x$colcoord

#' @rdname methods-ca
#' @export
recover_inertia.ca <- function(x) x$sv ^ 2

#' @rdname methods-ca
#' @export
recover_conference.ca <- function(x) {
  # `ca::ca()` always returns row and column standard coordinates
  c(0, 0)
}

#' @rdname methods-ca
#' @export
recover_coord.ca <- function(x) {
  colnames(x$rowcoord)
}

#' @rdname methods-ca
#' @export
augmentation_u.ca <- function(x) {
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

#' @rdname methods-ca
#' @export
augmentation_v.ca <- function(x){
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

#' @rdname methods-ca
#' @export
augmentation_coord.ca <- function(x){
  tibble(
    .name = factor_coord(recover_coord(x)),
    .sv = x$sv
  )
}
