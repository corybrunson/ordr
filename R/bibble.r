#' @rdname bibble
#' @export
bibble <- function(...) {
  x <- try(to_bibble(...))
  if (class(x) == "try-error") {
    return(make_bibble(...))
  } else {
    return(x)
  }
}

#' @rdname bibble
#' @export
make_bibble <- function(
  u = NULL, v = NULL, coordinates = NULL,
  ...
) {
  x <- c(list(u = u, v = v, coordinates = coordinates), list(...))
  attr(x, "preclass") <- NA
  to_bibble(x)
}

#' @rdname bibble
#' @export
as_bibble <- function(x) UseMethod("as_bibble")

#' @rdname bibble
#' @export
to_bibble <- function(x) UseMethod("to_bibble")

to_bibble.default <- function(x) {
  res <- list(
    u = get_u(x),
    v = get_v(x),
    coordinates = get_coordinates(x)
  )
  class(res) <- "bbl"
  attr(res, "preclass") <- setdiff(class(x), "bbl")
  res
}

#' @rdname bibble
#' @export
is.bibble <- function(x) {
  res <- inherits(x, "bbl") &
    all(coord_names(x) %in% names(get_u(x))) &
    all(coord_names(x) %in% names(get_v(x))) &
    (!is.null(attr(x, "preclass")) | !is.null(setdiff(class(x), "bbl")))
  res
}

#' @rdname bibble
#' @export
print.bbl <- function(x, n = 4, ...) {
  stopifnot(is.bibble(x))
  arg_list <- list(...)
  
  # components
  prev_class <- setdiff(c(attr(x, "preclass"), class(x)), "bbl")[1]
  wrapped <- is.null(attr(x, "preclass"))
  biplot_descr <- if (wrapped) {
    paste0("# A bibble-wrapped object of class '", prev_class, "':")
  } else if (!is.null(prev_class) && prev_class != "list") {
    paste0("# A bibble obtained from a class-'", prev_class, "' object:")
  } else {
    paste0("# A bibble:")
  }
  u <- get_u(x)
  u_trunc <- tibble::trunc_mat(u, n = n, ...)
  u_trunc$summary <- gsub("data\\.frame", "U", u_trunc$summary)
  v <- get_v(x)
  v_trunc <- tibble::trunc_mat(v, n = n, ...)
  v_trunc$summary <- gsub("data\\.frame", "V", v_trunc$summary)
  n_u <- nrow(u)
  n_v <- nrow(v)
  n_c <- nrow(get_coordinates(x))
  
  # print (would prefer to make this one print step)
  cat(
    biplot_descr,
    #n_u, " row", ifelse(n_u == 1, "", "s"), " and ",
    #n_v, " column", ifelse(n_v == 1, "", "s"), "\n",
    " ( ", n_u, " x ", n_c, " ) x ( ", n_v, " x ", n_c, " )'\n",
    "#   ", n_c, " coordinate", ifelse(n_c == 1, "", "s"), ": ",
    print_reps(get_coordinates(x)$.name), "\n",
    #if (wrapped) "" else paste0("#   Original class: '", prev_class, "'\n"),
    sep = ""
  )
  cat("# \n")
  print(u_trunc)
  cat("# \n")
  print(v_trunc)
  invisible(x)
}

get_u <- function(x) UseMethod("get_u")
get_v <- function(x) UseMethod("get_v")
get_coordinates <- function(x) UseMethod("get_coordinates")
clean_coordinates <- function(x) UseMethod("clean_coordinates")
get_data <- function(x) UseMethod("get_data")
get_data.ggvis <- ggvis::get_data

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

# FUNCTIONS TO EXTRACT U AND V AS MATRICES ON COORDINATES RATHER THAN TIBBLES
# OPTION TO SCALE?
factor_u <- function(x) UseMethod("factor_u")
factor_v <- function(x) UseMethod("factor_v")
factor_uv <- function(x, dimension) {
  uv <- switch(dimension, `1` = "u", `2` = "v")
  get_uv <- get(paste0("get_", uv))
  as.matrix(select(get_uv(x), pull(get_coordinates(x), .name)))
}
factor_u <- function(x) factor_uv(x, 1)
factor_v <- function(x) factor_uv(x, 2)

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

print_reps <- function(x) {
  x <- as.character(x)
  switch(
    min(length(x), 4),
    "1" = x,
    "2" = paste(x, collapse = " and "),
    "3" = paste(x, collapse = ", "),
    "4" = paste0(paste(x[1:2], collapse = ", "), ", ..., ", x[length(x)])
  )
}
