#' Extract the factors of an ordination object
#' 

#' The \code{get_*} functions extract one or both of the matrix factors 
#' \eqn{U,V} that constitute an ordination. These are interpreted as the subject
#' scores (\eqn{U}) and the variable loadings (\eqn{V}). \code{get_coord} 
#' retrieves the names of the coordinates shared by \eqn{U} and \eqn{V}.
#' 
#' The \code{*_annot} functions produce \link[tibble]{tibble}s of annotations 
#' for the subjects/scores, variables/loadings, and coordinates. The first field
#' of each tibble is \code{.name} and contains the subject, variable, and 
#' coordinate names, respectively.
#' 
#' \code{rank} retrieves the number of coordinates on which the data have been 
#' ordinated. The existing \code{\link[base]{rank}} function is made the default
#' and a new method is introduced for bibbles.
#' 

#' @name bibble-factors
#' @param x A bibble, or an object convertible to one.
#' @param .matrix A character string matching one of several indicators for one
#'   or both matrices in a matrix decomposition used for ordination. The
#'   standard values are \code{"u"}, \code{"v"}, and \code{"uv"}.
#'   

#' @rdname bibble-factors
#' @export
get_u <- function(x) UseMethod("get_u")
#' @rdname bibble-factors
#' @export
get_v <- function(x) UseMethod("get_v")
#' @rdname bibble-factors
#' @export
get_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = get_u(x),
    v = get_v(x),
    uv = list(u = get_u(x), v = get_v(x))
  )
}

as.matrix.bbl <- get_factor

#' @rdname bibble-factors
#' @export
get_coord <- function(x) UseMethod("get_coord")

# need 'get_*' functions before and after coercion; 'get_*.bbl' are unnecessary
get_u.default <- function(x) x$u
get_v.default <- function(x) x$v
get_coord.default <- function(x) {
  intersect(colnames(get_u(x)), colnames(get_v(x)))
}

# for fortified bibbles (also coordinates?)
get_u.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 1, -match(".matrix", names(x))]
}
get_v.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 2, -match(".matrix", names(x))]
}

#' @rdname bibble-factors
#' @export
u_annot <- function(x) UseMethod("u_annot")
#' @rdname bibble-factors
#' @export
v_annot <- function(x) UseMethod("v_annot")
#' @rdname bibble-factors
#' @export
factor_annot <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = u_annot(x),
    v = v_annot(x),
    uv = list(u = u_annot(x), v = v_annot(x))
  )
}
#' @rdname bibble-factors
#' @export
coord_annot <- function(x) UseMethod("coord_annot")

bibble_factors <- c(
  u = "u", v = "v", uv = "uv",
  U = "u", V = "v", UV = "uv",
  left = "u", right = "v", both = "uv",
  subjects = "u", variables = "v"
)
match_factor <- function(x) {
  x <- match.arg(x, names(bibble_factors))
  unname(bibble_factors[x])
}

#' @rdname bibble-factors
#' @export
rank <- function(x) UseMethod("rank")
rank.default <- base::rank
rank.bbl <- function(x) length(get_coord(x))
