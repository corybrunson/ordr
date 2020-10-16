
#' @rdname methods-lm
#' @export
reconstruct.lm <- function(x) {
  pred_mat <- as.matrix(x$model[, -1, drop = FALSE])
  names_fun <- if (class(x)[1] == "lm") names else rownames
  if (names_fun(x$coefficients)[1] == "(Intercept)") {
    pred_mat <- cbind(`(Intercept)` = 1, pred_mat)
  }
  coef_mat <- as.matrix(x$coefficients)
  if (class(x)[1] != "mlm") colnames(coef_mat) <- names(x$model)[1]
  as.data.frame(pred_mat %*% coef_mat)
}

#' @rdname methods-eigen
#' @export
reconstruct.eigen_ord <- function(x) {
  x[["vectors"]] %*% diag(x[["values"]]) %*% t(x[["vectors"]])
}

#' @rdname methods-svd
#' @export
reconstruct.svd_ord <- function(x) {
  x[["u"]] %*% diag(x[["d"]]) %*% t(x[["v"]])
}

#' @rdname methods-cmds
#' @export
reconstruct.cmds <- function(x) {
  -2 * x$points %*% t(x$points)
}

#' @rdname methods-kmeans
#' @export
reconstruct.kmeans <- function(x) {
  x$centers[x$cluster, , drop = FALSE]
}

#' @rdname methods-lpca
#' @export
reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}

#' @rdname methods-princomp
#' @export
reconstruct.princomp <- function(x) {
  res <- x[["scores"]] %*% t(x[["loadings"]])
  for (col in 1:ncol(res)) {
    for (row in 1:nrow(res)) {
      res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]
    }
  }
  res
}

#' @rdname methods-prcomp
#' @export
reconstruct.prcomp <- function(x) {
  res <- recover_u.prcomp(x)%*%t(recover_v.prcomp(x))
  if (x[["center"]] == FALSE && x[["scale"]] == FALSE) {
    res
  } else if (x[["center"]] != TRUE && x[["scale"]] == FALSE) {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- res[row, col] + x[["center"]][col]}}
    res
  } else {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]}}
    res
  }
}

#' @rdname methods-ca
#' @export
reconstruct.ca <- function(x) {
  std_resid <- x$rowcoord %*% diag(x$sv) %*% t(x$colcoord)
  stop("Not yet implemented.")
}
