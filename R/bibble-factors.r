#' Extract the factors of an ordination object
#' 

#' @name bibble-factors
#' @param x A bibble, or an object convertible to one.
#' @param .matrix A character string matching one of several indicators for one
#'   or both matrices in a matrix decomposition used for ordination. The
#'   standard values are \code{"u"} and \code{"v"}.
#'   

# REPLACE `get_*()` with `data_*()`?
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

#' @rdname bibble-factors
#' @export
get_coord <- function(x) UseMethod("get_coord")

get_u.default <- function(x) x$u
get_v.default <- function(x) x$v
get_coord.default <- function(x) x$coord
# need 'get_*' functions before and after coercion; 'get_*.bbl' are unnecessary

# for fortified bibbles
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
u_attr <- function(x) UseMethod("u_attr")
#' @rdname bibble-factors
#' @export
v_attr <- function(x) UseMethod("v_attr")
#' @rdname bibble-factors
#' @export
factor_attr <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = u_attr(x),
    v = v_attr(x),
    uv = list(u = u_attr(x), v = v_attr(x))
  )
}
#' @rdname bibble-factors
#' @export
coord_attr <- function(x) UseMethod("coord_attr")

bibble_factors <- c(
  u = "u", v = "v", uv = "uv",
  U = "u", V = "v", UV = "uv",
  left = "u", right = "v", both = "uv",
  observations = "u", variables = "v"
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
