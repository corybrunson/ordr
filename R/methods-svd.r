#' Functionality for singular value decompositions
#' 

svd <- function(x, LINPACK = FALSE){
  res <- base::svd(x = x, nu = min(nrow(x), ncol(x)), nv = min(nrow(x), ncol(x)), LINPACK = LINPACK)
  class(res) <- "svd"
  attr(res, "x") <- x
  res
}

as_tbl_ord.svd <- as_tbl_ord_default

recover_u.svd <- function(x){
  if (class(x) != "svd"){
    stop("recover_u() can only be called on a singular value decomposition output")
  }
  else {
    x$u
  }
}

recover_v.svd <- function(x){
  if (class(x) != "svd"){
    stop("recover_v() can only be called on a singular value decomposition output")
  }
  else {
    x$v
  }
}

recover_coord.svd <- function(x) paste0("SV", 1:ncol(x$u))

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

recompose_data_matrix <- function(x){
  if (class(x) != "svd"){
    stop("recompose_data_matrix() can only be called on a singular value decomposition output")
  }
  else {
    x$u %*% diag(x$d) %*% t(x$v)
  }
}
