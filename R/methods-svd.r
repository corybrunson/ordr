#' Functionality for singular value decompositions
#' 

get_singular_values <- function(x){
  if (all(names(x) != t(c("d", "u", "v")))){
    stop("get_singular_values() can only be called on a singular value decomposition output")
  }
  else {
    x$d
  }
}
