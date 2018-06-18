#' Bibble functionality for logistic principal components analysis and singular 
#' value decomposition objects
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"lpca"} and \code{"lsvd"} (see
#' \code{?logisticPCA::`logisticPCA-package`}). (This package masks the
#' signature functions of \strong{\link[logisticPCA]{logisticPCA}} with wrappers
#' that add row and column names from the input matrix to the output matrices.)
#' 
#' @name bibble-lpca
#' @template methods-params
#' @template matrix-param
#' @example inst/examples/ex-bibble-lpca.r

#' @importFrom stats plogis

#' @rdname bibble-lpca
#' @export
as_bibble.lpca <- as_bibble_recognized

recover_uv_lpca <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "PCs", v = "U")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname bibble-lpca
#' @export
recover_u.lpca <- function(x) recover_uv_lpca(x, "u")

#' @rdname bibble-lpca
#' @export
recover_v.lpca <- function(x) recover_uv_lpca(x, "v")

#' @rdname bibble-lpca
#' @export
recover_coord.lpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname bibble-lpca
#' @export
augment_u.lpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname bibble-lpca
#' @export
augment_v.lpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

#' @rdname bibble-lpca
#' @export
augment_coord.lpca <- function(x) {
  tibble(
    .name = recover_coord.lpca(x)
  )
}

#' @rdname bibble-lpca
#' @export
negate_to.lpca <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(recover_factor(as_bibble(x), .matrix), y)
  # tag 'lpca' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$U)))
  # return annotated object
  x
}

#' @rdname bibble-lpca
#' @export
as_bibble.lsvd <- as_bibble_recognized

recover_uv_lsvd <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "A", v = "B")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname bibble-lpca
#' @export
recover_u.lsvd <- function(x) recover_uv_lsvd(x, "u")

#' @rdname bibble-lpca
#' @export
recover_v.lsvd <- function(x) recover_uv_lsvd(x, "v")

#' @rdname bibble-lpca
#' @export
recover_coord.lsvd <- function(x) paste0("LSC", 1:ncol(x$A))

#' @rdname bibble-lpca
#' @export
augment_u.lsvd <- function(x) {
  tibble(
    .name = rownames(x$A)
  )
}

#' @rdname bibble-lpca
#' @export
augment_v.lsvd <- function(x) {
  tibble(
    .name = rownames(x$B),
    .mu = x$mu
  )
}

#' @rdname bibble-lpca
#' @export
augment_coord.lsvd <- function(x) {
  tibble(
    .name = recover_coord.lsvd(x)
  )
}

#' @rdname bibble-lpca
#' @export
reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}

#' @rdname bibble-lpca
#' @export
negate_to.lsvd <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_bibble(x), .matrix), y)
  # tag 'lsvd' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$B)))
  # return annotated object
  x
}
