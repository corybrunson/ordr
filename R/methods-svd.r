#' Functionality for singular value decompositions
#' 

svd <- function(x, LINPACK = FALSE){
  res <- base::svd(x = x, nu = min(nrow(x), ncol(x)), nv = min(nrow(x), ncol(x)), LINPACK = LINPACK)
  class(res) <- "SVD"
  attr(res, "x") <- x
  res
}

get_singular_values <- function(x){
  if (all(names(x) != t(c("d", "u", "v")))){
    stop("get_singular_values() can only be called on a singular value decomposition output")
  }
  else {
    x$d
  }
}

get_diagonal_matrix <- function(x){
  if (all(names(x) != t(c("d", "u", "v")))){
    stop("get_diagonal_matrix() can only be called on a singular value decomposition output")
  }
  else {
    diag(x$d)
  }
}

recompose_data_matrix <- function(x){
  if (all(names(x) != t(c("d", "u", "v")))){
    stop("recompose_data_matrix() can only be called on a singular value decomposition output")
  }
  else {
    x$u %*% diag(x$d) %*% t(x$v)
  }
}
