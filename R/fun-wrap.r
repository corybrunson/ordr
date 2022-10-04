#' @title Wrappers for lossy ordination methods
#'
#' @description These `*_ord` functions wrap core R functions with modifications
#'   for use with '[tbl_ord]' methods. Some parameters are hidden from the user
#'   and set to settings required for these methods, some matrix outputs are
#'   given row or column names to be used by them, and new '*_ord' S3 class
#'   attributes are added to enable them.
#'   

#' @details
#'
#' The following table summarizes the wrapped functions:
#'
#' | Original function     | Hide params | New params | Add names | New class |
#' | :-------------------- | :---------- | :--------- | :-------- | :-------- |
#' | [base::eigen()]       | Yes         | No         | Yes       | Yes       |
#' | [base::svd()]         | Yes         | No         | Yes       | Yes       |
#' | [stats::cmdscale()]   | Yes         | No         | No        | Yes       |
#' | [stats::cancor()]     | No          | Yes        | No        | Yes       |
#'
#' By default, [cancor_ord()] returns the same data as [stats::cancor()]: the
#' canonical correlations (`cor`), the canonical coefficients (`$xcoef` and
#' `$ycoef`), and the variable means (`$xcenter`, `$ycenter`). If `scores =
#' TRUE`, then [cancor_ord()] also returns the scores `$xscores` and `$yscores`
#' calculated from the (appropriately centered) data and the coefficients and
#' the intraset structure correlations `$xstructure` and `$ystructure` between
#' these and the data. These modifications are inspired by the `cancor()`
#' function in **candisc**, though two caveats should be noted: First, the
#' canonical coefficients (hence the canonical scores) are scaled by \eqn{n - 1}
#' compared to these, though the intraset structure correlations are the same.
#' Second, the _interset_ structure correlations are not returned, as these may
#' be obtained by conferring inertia unto the intraset ones.
#' 

#' @name wrap-ord
#' @include ord-tbl.r
#' @importFrom stats cmdscale
#' @inheritParams base::eigen
#' @inheritParams base::svd
#' @inheritParams stats::cmdscale
#' @inheritParams stats::cancor
#' @param scores Logical; whether to return canonical scores and structure
#'   correlations.
#' @return Objects slightly modified from the outputs of the original functions,
#'   with new '*-ord' classes.
#' @example inst/examples/ex-fun-wrap-glass.r
NULL

#' @rdname wrap-ord
#' @export
eigen_ord <- function(x, symmetric = isSymmetric.matrix(x)) {
  res <- eigen(x = x, symmetric = symmetric, only.values = FALSE)
  rownames(res$vectors) <- rownames(x)
  colnames(res$vectors) <- paste0("EV", seq_along(res$values))
  class(res) <- c("eigen_ord", class(res))
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
cancor_ord <- function(x, y, xcenter = TRUE, ycenter = TRUE, scores = FALSE) {
  res <- stats::cancor(x, y, xcenter = xcenter, ycenter = ycenter)
  if (scores) {
    xrk <- seq(ncol(res$xcoef))
    yrk <- seq(ncol(res$ycoef))
    res$xscores <-
      scale(x, center = xcenter, scale = FALSE)[, xrk, drop = FALSE] %*%
      res$xcoef
    res$yscores <-
      scale(y, center = ycenter, scale = FALSE)[, yrk, drop = FALSE] %*%
      res$ycoef
    res$xstructure <- stats::cor(x, res$xscores)
    res$ystructure <- stats::cor(y, res$yscores)
  }
  class(res) <- "cancor_ord"
  res
}
