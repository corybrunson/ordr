#' @title Confer inertia to factors of an ordination
#'
#' @description Re-distribute inertia between cases and variables in an
#'   ordination.
#'   

#' @details
#'
#' The _inertia_ of a singular value decomposition (eigendecomposition) consists
#' in the squares of the singular values (eigenvalues), and represents the
#' variance, likened to the physical inertia, in the directions of the
#' orthogonal singular vectors (eigenvectors). Read more about conferring
#' inertia between cases and variables at
#' <https://stats.stackexchange.com/a/141755/68743>.
#'
#' _Note: In case the `"inertia"` attribute is a rectangular matrix, one may
#' only be able to confer it entirely to the cases (`p = 1`) or entirely to the
#' variables (`p = 0`)._
#' 

#' @name conference
#' @include ord-augmentation.r
#' @param x A `tbl_ord` object.
#' @param p Numeric vector of length 1 or 2. If length 1, the proportion of the
#'   inertia assigned to the cases, with the remainder `1 - p` assigned to the
#'   variables. If length 2, the proportions of the inertia assigned to the
#'   cases and to the variables, respectively.
#' @example inst/examples/country-prcomp-confer.r
#' @example inst/examples/women-ca-confer.r
#' @example inst/examples/benthos-ca-augment-confer.r
NULL

#' @rdname conference
#' @export
recover_conference <- function(x) UseMethod("recover_conference")

#' @rdname conference
#' @export
recover_conference.default <- function(x) NULL

#' @rdname conference
#' @export
get_conference <- function(x) {
  if (is.null(attr(x, "confer"))) {
    return(recover_conference(x))
  } else {
    return(attr(x, "confer"))
  }
}

# in case it becomes expedient to switch from accessors to attributes
#recover_conference.tbl_ord <- function(x) attr(x, "confer")

#' @rdname conference
#' @export
confer_inertia <- function(x, p) {
  if (is.null(get_conference(x))) {
    stop(
      "Conference of inertia of `", deparse(substitute(x)), "` is not known."
    )
  }
  
  # don't manipulate or use singular values, but ensure that method exists
  recover_inertia(x)
  
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
