#' Functionality for principal components analysis ('princomp') objects
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"princomp"}.
#' 
#' @name methods-princomp
#' @template param-methods
#' @example inst/examples/ex-princomp.r

#' @rdname methods-princomp
#' @export
as_tbl_ord.princomp <- as_tbl_ord_default

#' @rdname methods-princomp
#' @export
recover_u.princomp <- function(x){
  x[["scores"]]
}

#' @rdname methods-princomp
#' @export
recover_v.princomp <- function(x){
  unclass(x[["loadings"]])
}

#' @rdname methods-princomp
#' @export
reconstruct.princomp <- function(x){
  res <- x[["scores"]]%*%t(x[["loadings"]])
  for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]}}
  res
}

#' @rdname methods-princomp
#' @export
recover_coord.princomp <- function(x){
  colnames(x[["scores"]])
}

#' @rdname methods-princomp
#' @export
augment_u.princomp <- function(x){
  .name <- rownames(x[["scores"]])
  if (is.null(.name)){
    tibble_pole(nrow(x[["x"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-princomp
#' @export
augment_v.princomp <- function(x){
  tibble(
    .name = colnames(x[["loadings"]]),
    .center = x[["center"]],
    .scale = x[["scale"]]
  )
}

#' @rdname methods-princomp
#' @export
augment_coord.princomp <- function(x){
  tibble(
    .name = recover_coord.princomp(x),
    .sdev = x[["sdev"]]
  )
}
