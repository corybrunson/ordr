# https://stats.stackexchange.com/a/141755/68743

#' Re-distribute inertia between cases and variables in an ordination
#' 
#' Note: In case \code{inertia} is a rectangular matrix, may only be able to 
#' confer it entirely to the cases (\code{p = 1}) or entirely to the variables
#' (\code{p = 0}).
#' 

#' @name conference
#' @include augmentation.r
#' @param x A \code{tbl_ord}.
#' @param p Numeric vector of length 1 or 2. If length 1, the proportion of the 
#'   inertia assigned to the cases, with the remainder \code{1 - p} assigned to 
#'   the variables. If length 2, the proportions of the inertia assigned to the 
#'   cases and to the variables, respectively.

#' @rdname conference
#' @export
confer_inertia <- function(x, p) {
  # don't manipulate or use singular values, but ensure that method exists
  sv <- recover_sv(x)
  
  # interpret `p`
  if (! is.numeric(p)) {
    p <- switch_inertia(p)
  } else {
    if (length(p) > 2) {
      stop("`p` must have length 1 or 2.")
    }
    if (any(p < 0) | any(p > 1)) {
      stop("Inertias must be proportions.")
    }
    if (length(p) == 2) {
      if (sum(p) != 1) {
        warning("Inertias are not balanced.")
      }
    }
    if (length(p) == 1) {
      p <- c(p, 1 - p)
    }
  }
  
  # add attribute
  x <- attribute_conference(x, p)
  x
}

attribute_conference <- function(x, p) {
  attr(x, "confer") <- p
  x
}

recover_inertia_u <- function(x) {
  #u <- recover_u(x)
  #diag(
  #  apply(u, 2, norm, type = "2"),
  #  nrow = ncol(u), ncol = ncol(u)
  #)
  if (is.null(recover_inertia(x))) {
    0
  } else {
    recover_inertia(x)[1]
  }
}

recover_inertia_v <- function(x) {
  if (is.null(recover_inertia(x))) {
    0
  } else {
    recover_inertia(x)[2]
  }
}
