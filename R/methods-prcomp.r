#' Functionality for principal components analysis ('prcomp') objects
#' 

as_tbl_ord.prcomp <- as_tbl_ord_default

recover_u.prcomp <- function(x){
  x[["x"]]
}

recover_v.prcomp <- function(x){
  x[["rotation"]]
}

reconstruct.prcomp <- function(x){
  res <- recover_u.prcomp(x)%*%t(recover_v.prcomp(x))
  if (x[["center"]] == FALSE && x[["scale"]] == FALSE){
    res
  } else if (x[["center"]] != TRUE && x[["scale"]] == FALSE) {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- res[row, col] + x[["center"]][col]}}
    res
  } else {
    for (col in 1:ncol(res)) {for (row in 1:nrow(res)) {res[row, col] <- (res[row, col] * x[["scale"]][col]) + x[["center"]][col]}}
    res
  }
}

recover_coord.prcomp <- function(x){
  colnames(x[["rotation"]])
}

augment_u.prcomp <- function(x){
  tibble(
    .name = rownames(x[["x"]])
  )
}

augment_v.prcomp <- function(x){
  res <- tibble(.name = rownames(x[["rotation"]]))
  if (class(x[["center"]]) == "numeric"){
    res <- dplyr::bind_cols(res, .centering = x[["center"]])
  }
  if (class(x[["scale"]]) == "numeric"){
    res <- dplyr::bind_cols(res, .scaling = x[["scale"]])
  }
  res
}

augment_coord.prcomp <- function(x)
{
  tibble(
    .name = recover_coord(x),
    .sdev = x[["sdev"]]
  )
}
