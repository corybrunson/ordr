#' @title Masks of external ordination objects
#'
#' @description Some popular ordination functions in R return objects without a
#'   specific class (e.g. [stats::cmdscale()]). Some others return output that
#'   is not self-contained, for instance missing annotation from the original
#'   dataset (e.g. [logisticPCA::logisticPCA()]). The functions below wrap the
#'   originals with the minimal overhead necessary for complete functionality
#'   with the [tbl_ord] class.

#' @name masks
#' @importFrom logisticPCA logisticPCA logisticSVD
#' @inheritParams stats::cmdscale
#' @inheritParams logisticPCA::logisticPCA
#' @inheritParams logisticPCA::logisticSVD
#' @inheritParams logisticPCA::convexLogisticPCA
#' @param ... Additional parameters passed to original functions.

#' @rdname masks
#' @export
cmdscale <- function(d, k = 2, add = FALSE) {
  res <- stats::cmdscale(d, k = k, eig = TRUE, add = add, x.ret = TRUE)
  class(res) <- "cmds"
  res
}

#' @rdname masks
#' @export
logisticSVD <- function(
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

#' @rdname masks
#' @export
logisticPCA <- function(
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

#' @rdname masks
#' @export
convexLogisticPCA <- function(
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
