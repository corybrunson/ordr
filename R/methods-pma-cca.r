#' @title Functionality for penalized multivariate analysis ('SPC', 'CCA',
#'   'MultiCCA') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class 'SPC', 'CCA', and 'MultiCCA' from the
#'   **[PMA][PMA::PMA-package]** package.
#'
#' @details
#'
#' Witten, Tibshirani, and Hastie (2009) provide a theoretical basis and
#' computational algorithm for penalized matrix decomposition that specializes
#' to sparse PCA and to sparse CCA. Their R package **PMA** implements these
#' specializations as well as one to sparse multiple CCA.
#'
#' **Note:** These draft methods produce the biplot of Greenacre (1984), which
#' are advised against by ter Braak (1990).
#' 

#' @template ref-witten2009
#' @template ref-greenacre1984
#' @template ref-braak1990

#' @name methods-pma
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-pma-cca-savings.r
NULL

#' @rdname methods-pma
#' @export
as_tbl_ord.CCA <- as_tbl_ord_default

#' @rdname methods-pma
#' @export
recover_coord.CCA <- function(x) {
  paste0("sCD", seq(x$K))
}

#' @rdname methods-pma
#' @export
recover_rows.CCA <- function(x) {
  res <- x$u
  if (! is.null(x$xnames)) rownames(res) <- x$xnames
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-pma
#' @export
recover_cols.CCA <- function(x) {
  res <- x$v
  if (! is.null(x$znames)) rownames(res) <- x$znames
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-pma
#' @export
recover_inertia.CCA <- function(x) x$cors^2

#' @rdname methods-pma
#' @export
recover_conference.CCA <- function(x) {
  # `u`, `v`, and `d` comprise the singular value decomposition
  c(0, 0)
}

#' @rdname methods-pma
#' @export
augmentation_rows.CCA <- function(x) {
  .name <- x$xnames
  if (is.null(.name)) {
    tibble_pole(nrow(x$u))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-pma
#' @export
augmentation_cols.CCA <- function(x) {
  .name <- x$znames
  if (is.null(.name)) {
    tibble_pole(nrow(x$v))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-pma
#' @export
augmentation_coord.CCA <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .cancor = x$cors
  )
}
