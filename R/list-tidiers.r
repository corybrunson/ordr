#' Tidying methods for lists, including classical multidimensional scaling
#'
#' When [cmdscale()] is instructed to return any of several optional elements,
#' or when `list. = TRUE`, the output is not the default point coordinate matrix
#' but a 5-element list with a consistent naming scheme (though some elements
#' will be `NULL` if their parameters are not set to `TRUE`). Some unclassed
#' lists of fixed structure, returned by other functions, are recognized by
#' **broom**'s [list tidiers][broom::list_tidiers]. **ordr**'s updated list
#' tidiers append an additional check for CMDS list structure. If the CMDS list
#' structure is detected, then the in-package tidiers [tidy_cmdscale()] and
#' [glance_cmdscale()] are called.
#'
#' @inheritParams broom::list_tidiers
#' @inheritParams broom::`tidy_svd`
#' @name list_tidiers
#' @export
#' @family list tidiers
tidy.list <- function(x, ...) {
  optim_elems <- c("par", "value", "counts", "convergence", "message")
  xyz_elems <- c("x", "y", "z")
  svd_elems <- c("d", "u", "v")
  irlba_elems <- c(svd_elems, "iter", "mprod")
  cmdscale_elems <- c("points", "eig", "x", "ac", "GOF")
  
  if (all(optim_elems %in% names(x))) {
    tidy_optim(x, ...)
  } else if (all(xyz_elems %in% names(x))) {
    tidy_xyz(x, ...)
  } else if (all(irlba_elems %in% names(x))) {
    tidy_irlba(x, ...)
  } else if (all(svd_elems %in% names(x))) {
    tidy_svd(x, ...)
  } else if (all(cmdscale_elems %in% names(x))) {
    tidy_cmdscale(x, ...)
  } else {
    stop("No tidy method recognized for this list.", call. = FALSE)
  }
}

#' @rdname list_tidiers
#' @export
glance.list <- function(x, ...) {
  optim_elems <- c("par", "value", "counts", "convergence", "message")
  cmdscale_elems <- c("points", "eig", "x", "ac", "GOF")
  
  if (all(optim_elems %in% names(x))) {
    glance_optim(x, ...)
  } else if (all(cmdscale_elems %in% names(x))){
    glance_cmdscale(x, ...)
  } else {
    stop("No glance method recognized for this list.", call. = FALSE)
  }
}

tidy_optim <- getFromNamespace("tidy_optim", "broom")
tidy_xyz <- getFromNamespace("tidy_xyz", "broom")
tidy_irlba <- getFromNamespace("tidy_irlba", "broom")
tidy_svd <- getFromNamespace("tidy_svd", "broom")
glance_optim <- getFromNamespace("glance_optim", "broom")
