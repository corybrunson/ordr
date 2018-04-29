#' Extract the factors of a biplot object

#' @name bibble-factors
#' @param x A bibble, or an object convertible to one.

#' @rdname bibble-factors
#' @export
get_u <- function(x) UseMethod("get_u")
#' @rdname bibble-factors
#' @export
get_v <- function(x) UseMethod("get_v")
#' @rdname bibble-factors
#' @export
get_uv <- function(x, matrix = "uv") {
  matrix <- match.arg(matrix, names(bibble_factors))
  switch(
    as.list(bibble_factors)[[matrix]],
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

factor_uv <- function(x, matrix = "uv") {
  matrix <- match.arg(matrix, names(bibble_factors))
  uv <- as.list(bibble_factors)[[matrix]]
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
factor_u <- function(x) factor_uv(x, "u")
#' @rdname bibble-factors
#' @export
factor_v <- function(x) factor_uv(x, "u")
#' @rdname bibble-factors
#' @export

bibble_factors <- c(
  u = "u", v = "v", uv = "uv",
  U = "u", V = "v", UV = "uv",
  left = "u", right = "v", both = "uv",
  observations = "u", variables = "v"
)

set_coordinates <- function(x, ...) {
  x$coordinates <- data.frame(.name = unlist(list(...)))
  x
}

guess_coordinates <- function(x) {
  intersect(names(get_u(x)), names(get_v(x)))
}

u_names <- function(x) {
  get_u(x)$.name
}
`u_names<-` <- function(x, value) {
  x$u$.name <- value
  x
}
v_names <- function(x) {
  get_v(x)$.name
}
`v_names<-` <- function(x, value) {
  x$v$.name <- value
  x
}
coord_names <- function(x) {
  get_coordinates(x)$.name
}
`coord_names<-` <- function(x, value) {
  x$coordinates$.name <- value
  u_names(x) <- value
  v_names(x) <- value
  x
}

get_data <- function(x) UseMethod("get_data")
get_data.ggvis <- ggvis::get_data
