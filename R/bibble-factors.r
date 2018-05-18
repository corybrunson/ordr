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
get_uv <- function(x, .matrix = "uv") {
  switch(
    match_factor(.matrix),
    u = get_u(x),
    v = get_v(x),
    uv = list(u = get_u(x), v = get_v(x))
  )
}
#' @rdname bibble-factors
#' @export
get_coordinates <- function(x) UseMethod("get_coordinates")
#' @rdname bibble-factors
#' @export
clean_coordinates <- function(x) UseMethod("clean_coordinates")

#' @rdname bibble-factors
#' @export
rank <- function(x) UseMethod("rank")
rank.default <- base::rank
rank.bbl <- function(x) length(get_coordinates(x))

get_u.default <- function(x) as_tibble(as.data.frame(x$u))
get_v.default <- function(x) as_tibble(as.data.frame(x$v))
get_coordinates.default <- function(x) as_tibble(as.data.frame(x$coordinates))
# need 'get_*' functions before and after coercion; 'get_*.bbl' are unnecessary

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
factor_uv <- function(x, .matrix = "uv") {
  uv <- match_factor(.matrix)
  get_fun <- get(paste0("get_", uv))
  if (uv == "uv") {
    return(lapply(
      get_fun(x),
      select,
      pull(get_coordinates(x), .name)
    ))
  } else {
    return(select(get_fun(x), pull(get_coordinates(x), .name)))
  }
}
#' @rdname bibble-factors
#' @export
factor_u <- function(x) factor_uv(x, "u")
#' @rdname bibble-factors
#' @export
factor_v <- function(x) factor_uv(x, "u")

#' @rdname bibble-factors
#' @export
attr_uv <- function(x, .matrix = "uv") {
  uv <- match_factor(.matrix)
  get_fun <- get(paste0("get_", uv))
  if (uv == "uv") {
    return(lapply(
      get_fun(x),
      select,
      -one_of(pull(get_coordinates(x), .name))
    ))
  } else {
    return(select(get_fun(x), -one_of(pull(get_coordinates(x), .name))))
  }
}
#' @rdname bibble-factors
#' @export
attr_u <- function(x) attr_uv(x, "u")
#' @rdname bibble-factors
#' @export
attr_v <- function(x) attr_uv(x, "u")

#' @rdname bibble-factors
#' @export
matrix_uv <- function(x, .matrix = "uv") {
  uv <- match_factor(.matrix)
  get_fun <- get(paste0("get_", uv))
  if (uv == "uv") {
    return(lapply(lapply(
      get_fun(x),
      select,
      pull(get_coordinates(x), .name)
    ), as.matrix))
  } else {
    return(as.matrix(select(get_fun(x), pull(get_coordinates(x), .name))))
  }
}
#' @rdname bibble-factors
#' @export
matrix_u <- function(x) matrix_uv(x, "u")
#' @rdname bibble-factors
#' @export
matrix_v <- function(x) matrix_uv(x, "u")

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
