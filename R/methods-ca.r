#' Functionality for correspondence analysis ('ca') objects
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"ca"} from the \strong{ca} package.
#' 
#' @name methods-ca
#' @template param-methods
#' @example inst/examples/ex-ca.r

#' @rdname methods-ca
#' @export
as_tbl_ord.ca <- as_tbl_ord_default

#' @rdname methods-ca
#' @export
reconstruct.ca <- function(x) {
  std_resid <- x$rowcoord %*% diag(x$sv) %*% t(x$colcoord)
  stop("Not yet implemented.")
}

#' @rdname methods-ca
#' @export
recover_u.ca <- function(x) x$rowcoord

#' @rdname methods-ca
#' @export
recover_v.ca <- function(x) x$colcoord

#' @rdname methods-ca
#' @export
recover_inertia.ca <- function(x) x$sv

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
augment_u.ca <- function(x) {
  tibble(
    .name = rownames(x$rowcoord),
    .mass = x$rowmass,
    .dist = x$rowdist,
    .inertia = x$rowinertia
  )
}

#' @rdname methods-ca
#' @export
augment_v.ca <- function(x){
  tibble(
    .name = rownames(x$colcoord),
    .mass = x$colmass,
    .dist = x$coldist,
    .inertia = x$colinertia
  )
}

#' @rdname methods-ca
#' @export
augment_coord.ca <- function(x){
  tibble(
    .name = recover_coord(x),
    .sv = x$sv
  )
}
