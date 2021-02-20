#' @title Functionality for logistic PCA and logistic SVD objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"lpca"` and `"lsvd"` from the
#'   **[logisticPCA][logisticPCA::logisticPCA-package]** package. The signature
#'   functions [logisticPCA::logisticPCA()], [logisticPCA::logisticSVD()], and
#'   [logisticPCA::convexLogisticPCA()] have `*_ord()` wrappers that add row and
#'   column names from the input matrix to the output matrices.
#'
#' @name methods-lpca
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/finches-lpca-secondary.r
NULL

#' @importFrom stats plogis

#' @rdname methods-lpca
#' @export
as_tbl_ord.lsvd <- as_tbl_ord_default

recover_uv_lsvd <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "A", v = "B")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lpca
#' @export
recover_u.lsvd <- function(x) recover_uv_lsvd(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.lsvd <- function(x) recover_uv_lsvd(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.lsvd <- function(x) paste0("LSC", 1:ncol(x$A))

#' @rdname methods-lpca
#' @export
augmentation_u.lsvd <- function(x) {
  tibble(
    .name = rownames(x$A)
  )
}

#' @rdname methods-lpca
#' @export
augmentation_v.lsvd <- function(x) {
  tibble(
    .name = rownames(x$B),
    .mu = x$mu
  )
}

#' @rdname methods-lpca
#' @export
augmentation_coord.lsvd <- function(x) {
  tibble(
    .name = factor_coord(recover_coord.lsvd(x))
  )
}

#' @rdname methods-lpca
#' @export
as_tbl_ord.lpca <- as_tbl_ord_default

recover_uv_lpca <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "PCs", v = "U")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lpca
#' @export
recover_u.lpca <- function(x) recover_uv_lpca(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.lpca <- function(x) recover_uv_lpca(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.lpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname methods-lpca
#' @export
augmentation_u.lpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-lpca
#' @export
augmentation_v.lpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

#' @rdname methods-lpca
#' @export
augmentation_coord.lpca <- function(x) {
  tibble(
    .name = factor_coord(recover_coord.lpca(x))
  )
}

#' @rdname methods-lpca
#' @export
as_tbl_ord.clpca <- as_tbl_ord_default

#' @rdname methods-lpca
#' @export
recover_u.clpca <- function(x) recover_uv_lpca(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.clpca <- function(x) recover_uv_lpca(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.clpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname methods-lpca
#' @export
augmentation_u.clpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-lpca
#' @export
augmentation_v.clpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

#' @rdname methods-lpca
#' @export
augmentation_coord.clpca <- function(x) {
  tibble(
    .name = factor_coord(recover_coord.clpca(x))
  )
}
