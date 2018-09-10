#' Functionality for singular value decompositions
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"svd"}. (This is a class introduced in this package to identify 
#' objects returned by \code{\link{svd}}, which is masked by a wrapper that
#' adds the class attribute.)
#' 
#' @name methods-svd
#' @template param-methods

#' @rdname methods-svd
#' @export
as_tbl_ord.svd <- as_tbl_ord_default

#' @rdname methods-svd
#' @export
recover_u.svd <- function(x){
  res <- x$u
  colnames(res) <- recover_coord.svd(x)
  rownames(res) <- rownames(attr(x, "x"))
  res
}

#' @rdname methods-svd
#' @export
recover_v.svd <- function(x){
  res <- x$v
  colnames(res) <- recover_coord.svd(x)
  rownames(res) <- t(colnames(attr(x, "x")))
  res
}

#' @rdname methods-svd
#' @export
recover_coord.svd <- function(x) paste0("SV", 1:ncol(x$u))

#' @rdname methods-svd
#' @export
augment_u.svd <- function(x){
  .name <- rownames(attr(x, "x"))
  if (is.null(.name)) {
    tibble_pole(nrow(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augment_v.svd <- function(x){
  .name <- colnames(attr(x, "x"))
  if (is.null(.name)) {
    tibble_pole(ncol(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augment_coord.svd <- function(x){
  tibble(
    .name = recover_coord(x),
    .value = x$d[1:ncol(x$u)]
  )
}

#' @rdname methods-svd
#' @export
reconstruct.svd <- function(x){
  x$u %*% diag(x$d) %*% t(x$v)
}

get_singular_values <- function(x){
  if (class(x) != "svd"){
    stop("get_singular_values() can only be called on a singular value decomposition output")
  }
  else {
    x$d
  }
}

get_diagonal_matrix <- function(x){
  if (class(x) != "svd"){
    stop("get_diagonal_matrix() can only be called on a singular value decomposition output")
  }
  else {
    diag(x$d)
  }
}
