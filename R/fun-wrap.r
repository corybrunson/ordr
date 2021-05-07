#' @title Wrappers for lossy ordination methods
#'
#' @description These `*_ord` functions wrap functions imported from other
#'   packages with modifications for use with '[tbl_ord]' methods. Some
#'   parameters are hidden from the user and set to settings required for these
#'   methods, some matrix outputs are given row or column names to be used by
#'   them, and new '*_ord' S3 class attributes are added to enable them.
#'   

#' @details
#' 
#' The following table summarizes the wrapped functions:
#' 
#' | Original function                  | Hide params | Add names | New class |
#' | :--------------------------------- | :---------- | :-------- | :-------- |
#' | [base::eigen()]                    | Yes         | Yes       | Yes       |
#' | [base::svd()]                      | Yes         | Yes       | Yes       |
#' | [stats::cmdscale()]                | Yes         | No        | Yes       |
#' | [logisticPCA::logisticSVD()]       | No          | Yes       | No        |
#' | [logisticPCA::logisticPCA()]       | No          | Yes       | No        |
#' | [logisticPCA::convexLogisticPCA()] | No          | Yes       | No        |

#' @name wrap-ord
#' @include ord-tbl.r
#' @importFrom stats cmdscale
#' @importFrom logisticPCA logisticPCA logisticSVD convexLogisticPCA
#' @inheritParams base::eigen
#' @inheritParams base::svd
#' @inheritParams stats::cmdscale
#' @inheritParams logisticPCA::logisticPCA
#' @inheritParams logisticPCA::logisticSVD
#' @inheritParams logisticPCA::convexLogisticPCA
#' @param ... Additional parameters passed to original functions.
NULL

#' @rdname wrap-ord
#' @export
eigen_ord <- function(x, symmetric = isSymmetric.matrix(x)) {
  res <- eigen(x = x, only.values = FALSE)
  rownames(res$vectors) <- rownames(x)
  colnames(res$vectors) <- paste0("EV", seq_along(res$values))
  class(res) <- "eigen_ord"
  res
}

#' @rdname wrap-ord
#' @export
svd_ord <- function(x, nu = min(dim(x)), nv = min(dim(x))) {
  res <- svd(x = x, nu = nu, nv = nv)
  rownames(res$u) <- rownames(x)
  colnames(res$u) <- paste0("SV", seq_along(res$d))
  rownames(res$v) <- colnames(x)
  colnames(res$v) <- paste0("SV", seq_along(res$d))
  class(res) <- "svd_ord"
  res
}

#' @rdname wrap-ord
#' @export
cmdscale_ord <- function(d, k = 2, add = FALSE) {
  res <- stats::cmdscale(d, k = k, eig = TRUE, add = add, x.ret = TRUE)
  class(res) <- "cmds_ord"
  res
}

#' @rdname wrap-ord
#' @export
logisticSVD_ord <- function(
  x, k = 2,
  quiet = TRUE, max_iters = 1000,
  conv_criteria = 1e-05, random_start = FALSE,
  ...,
  partial_decomp = TRUE, main_effects = TRUE
) {
  lsvd <- logisticPCA::logisticSVD(
    x = x, k = k,
    quiet = quiet, max_iters = max_iters,
    conv_criteria = conv_criteria, random_start = random_start,
    ...,
    partial_decomp = partial_decomp, main_effects = main_effects
  )
  rownames(lsvd$A) <- rownames(x)
  rownames(lsvd$B) <- colnames(x)
  lsvd
}

#' @rdname wrap-ord
#' @export
logisticPCA_ord <- function(
  x, k = 2, m = 4,
  quiet = TRUE, partial_decomp = FALSE,
  max_iters = 1000, conv_criteria = 1e-05,
  random_start = FALSE,
  ...,
  main_effects = TRUE
) {
  lpca <- logisticPCA::logisticPCA(
    x, k = k,
    m = m, quiet = quiet, partial_decomp = partial_decomp,
    max_iters = max_iters, conv_criteria = conv_criteria,
    random_start = random_start,
    ...,
    main_effects = main_effects
  )
  rownames(lpca$U) <- colnames(x)
  #rownames(lpca$PCs) <- rownames(x)
  lpca
}

#' @rdname wrap-ord
#' @export
convexLogisticPCA_ord <- function(
  x, k = 2, m = 4,
  quiet = TRUE, partial_decomp = FALSE,
  max_iters = 1000, conv_criteria = 1e-06,
  random_start = FALSE,
  ...,
  main_effects = TRUE, ss_factor = 4
) {
  lpca <- logisticPCA::convexLogisticPCA(
    x, k = k,
    m = m, quiet = quiet, partial_decomp = partial_decomp,
    max_iters = max_iters, conv_criteria = conv_criteria,
    random_start = random_start,
    ...,
    main_effects = main_effects, ss_factor = 4
  )
  rownames(lpca$U) <- colnames(x)
  #rownames(lpca$PCs) <- rownames(x)
  lpca
}
