#' @title Tidying methods for (additional) lists
#'
#' @description Some unclassed lists of fixed structure, returned by other
#'   functions, are recognized by **broom**'s [list
#'   tidiers][broom::list_tidiers]. **ordr**'s updated list tidiers append
#'   additional checks for the list structures returned by [cmdscale()] and
#'   [cancor()]. If such structure is detected, then the new in-package tidiers
#'   are called.
#'
#' @name list_tidiers
#' @include ord-tidiers.r
#' @importFrom broom tidy_irlba
#' @inheritParams broom::list_tidiers
#' @inheritParams broom::svd_tidiers
#' @template return-tidier
#' @family list tidiers
#' @seealso [generics::tidy()] [generics::glance()]
#' @export
tidy.list <- function(x, ...) {
  optim_elems <- c("par", "value", "counts", "convergence", "message")
  xyz_elems <- c("x", "y", "z")
  svd_elems <- c("d", "u", "v")
  irlba_elems <- c(svd_elems, "iter", "mprod")
  cmdscale_elems <- c("points", "eig", "x", "ac", "GOF")
  cancor_elems <- c("cor", "xcoef", "ycoef", "xcenter", "ycenter")
  
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
  } else if (all(cancor_elems %in% names(x))) {
    tidy_cancor(x, ...)
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
# tidy_irlba <- getFromNamespace("tidy_irlba", "broom")
tidy_svd <- getFromNamespace("tidy_svd", "broom")
glance_optim <- getFromNamespace("glance_optim", "broom")

#' @importFrom utils globalVariables
globalVariables(c("PC", "std.dev", "percent", "column"))
# -+- This is not ideal but used to pre-empt a CRAN NOTE. -+-

as_glance_tibble <- getFromNamespace("as_glance_tibble", "broom")
