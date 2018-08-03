#' Functionality for principal components analysis ('princomp') objects
#' 

# 'as_tbl_ord_default' cannot be used because class 'loadings' cannot be coerced to a data.frame
as_tbl_ord.princomp <- as_tbl_ord_default

recover_u.princomp <- function(x){
  x[["scores"]]
}

recover_v.princomp <- function(x){
  unclass(x[["loadings"]])
}

reconstruct.princomp <- function(x){
  res <- x[["scores"]]%*%t(x[["loadings"]])
  for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]}}
  res
}

recover_coord.princomp <- function(x){
  colnames(x[["scores"]])
}

augment_u.princomp <- function(x){
  .name <- rownames(x[["scores"]])
  if (is.null(.name)){
    tibble_pole(nrow(x[["x"]]))
  } else {
    tibble(.name = .name)
  }
}

augment_v.princomp <- function(x){
  tibble(
    .name = colnames(x[["loadings"]]),
    .center = x[["center"]],
    .scale = x[["scale"]]
  )
}

augment_coord.princomp <- function(x){
  tibble(
    .name = recover_coord.princomp(x),
    .sdev = x[["sdev"]]
  )
}
