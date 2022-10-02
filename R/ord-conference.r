#' @title Confer inertia to factors of a 'tbl_ord' object
#'
#' @description Re-distribute inertia between rows and columns in an
#'   ordination.
#'   

#' @details
#'
#' The _inertia_ of a singular value decomposition \eqn{X=UDV'} consists in the
#' squares of the singular values (the diagonal elements of \eqn{D}), and
#' represents the variance, likened to the physical inertia, in the directions
#' of the orthogonal singular vectors (the columns of \eqn{U} or of \eqn{V}).
#' Biplots superimpose the projections of the rows and the columns of \eqn{X}
#' onto these coordinate vectors, scaled by some proportion of the total
#' inertia: \eqn{UD^p} and \eqn{VD^q}. A biplot is _balanced_ if \eqn{p+q=1}.
#' Read Orlov (2013) for more on conferring inertia in PCA.
#'
#' `recover_conference()`, like the other recoverers, is an [S3
#' method][base::S3Methods] that is exported for convenience but not intended to
#' be used directly.
#'
#' _Note: In case the `"inertia"` attribute is a rectangular matrix, one may
#' only be able to confer it entirely to the cases (`p = 1`) or entirely to the
#' variables (`p = 0`)._
#' 

#' @template ref-orlov2013

#' @name conference
#' @include ord-augmentation.r
#' @param x A [tbl_ord].
#' @param p Numeric vector of length 1 or 2. If length 1, the proportion of the
#'   inertia assigned to the cases, with the remainder `1 - p` assigned to the
#'   variables. If length 2, the proportions of the inertia assigned to the
#'   cases and to the variables, respectively.
#' @return `recover_conference()` returns the (statically implemented)
#'   distribution of inertia between the rows and the columns as stored in the
#'   model. `confer_inertia()` returns a tbl_ord with a specified distribution
#'   of inertia but the wrapped model unchanged. `get_conference()` returns the
#'   distribution currently conferred.
#' @example inst/examples/ex-ord-conference.r
#' @family generic recoverers
NULL

attribute_conference <- function(x, p) {
  attr(x, "confer") <- p
  x
}

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

#' @rdname conference
#' @export
revert_conference <- function(x) attribute_conference(x, NULL)

# in case it becomes expedient to switch from recoverers to attributes
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
      stop("`p` must be one or two proportions.")
    }
    if (length(p) == 2) {
      if (sum(p) != 1) {
        warning("Inertia is not balanced.")
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
