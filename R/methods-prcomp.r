#' Functionality for prcomp objects
#' 

get_rotated_data <- function(obj){
  if (class(obj) != "prcomp"){
    stop("get_rotated_data() can only be called on an object of class prcomp")
  }
  else {
    obj$x
  }
}
