
pull_u <- function(x, var = -1) {
  stopifnot(class(x)[1] == "bbl")
  pull(get_uv(x, matrix = "u"), !!enquo(var))
}

pull_v <- function(x, var = -1) {
  stopifnot(class(x)[1] == "bbl")
  pull(get_uv(x, matrix = "v"), !!enquo(var))
}

mutate.bbl <- function(x, matrix = NULL, ...) {
  x <- to_bibble(x)
  # don't allow mutations of coordinates
  if (any(names(quos(...)) %in% get_coordinates(x))) {
    stop("The coordinates of `x` are immutable.")
  }
  # if 'matrix' is NULL, try each mutation in both factors
  if (is.null(matrix)) {
    for (matrix in c("u", "v")) {
      dots <- quos(...)
      d <- get_uv(x, matrix = matrix)
      for (i in seq_along(dots)) {
        dot <- dots[i]
        d_tmp <- try(mutate(d, !!!dot))
        if (class(d_tmp)[1] == "try-error") next
        d <- d_tmp
      }
      x[[matrix]] <- d
    }
  } else if (matrix == "uv") {
    for (matrix in c("u", "v")) {
      d <- get_uv(x, matrix = matrix)
      x[[matrix]] <- mutate(d, ...)
    }
  } else {
    d <- get_uv(x, matrix = matrix)
    x[[matrix]] <- mutate(d, ...)
  }
  x
}

mutate_u <- function(x, ...) {
  stopifnot(class(x)[1] == "bbl")
  mutate.bbl(x, matrix = "u", ...)
}

mutate_v <- function(x, ...) {
  stopifnot(class(x)[1] == "bbl")
  mutate.bbl(x, matrix = "v", ...)
}

inner_join.bbl <- function(
  x, matrix = NULL, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (is.null(matrix)) {
    matrix <- ""
    if (length(intersect(names(x$u), names(y))) > 0)
      matrix <- paste0(matrix, "u")
    if (length(intersect(names(x$v), names(y))) > 0)
      matrix <- paste0(matrix, "v")
    if (matrix == "")
      stop("`by` required, because the data sources have no common variables.")
  }
  if (matrix == "uv") {
    for (matrix in c("u", "v")) {
      d <- get_uv(x, matrix = matrix)
      x[[matrix]] <- inner_join(d, y, by = by, copy = copy, suffix = suffix)
      if (nrow(x[[matrix]]) == 0)
        stop("No matching values for `by` variables in matrix '", matrix, "'.")
    }
  } else {
    d <- get_uv(x, matrix = matrix)
    x[[matrix]] <- inner_join(d, y, by = by, copy = copy, suffix = suffix)
  }
  x
}

inner_join_u <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  inner_join.bbl(x, matrix = "u", y = y, by = by, copy = copy, suffix = suffix)
}

inner_join_v <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  inner_join.bbl(x, matrix = "v", y = y, by = by, copy = copy, suffix = suffix)
}

left_join.bbl <- function(
  x, matrix = NULL, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (is.null(matrix)) {
    matrix <- ""
    if (length(intersect(names(x$u), names(y))) > 0)
      matrix <- paste0(matrix, "u")
    if (length(intersect(names(x$v), names(y))) > 0)
      matrix <- paste0(matrix, "v")
    if (matrix == "")
      stop("`by` required, because the data sources have no common variables.")
  }
  if (matrix == "uv") {
    for (matrix in c("u", "v")) {
      d <- get_uv(x, matrix = matrix)
      x[[matrix]] <- left_join(d, y, by = by, copy = copy, suffix = suffix)
      if (nrow(x[[matrix]]) == 0)
        stop("No matching values for `by` variables in matrix '", matrix, "'.")
    }
  } else {
    d <- get_uv(x, matrix = matrix)
    x[[matrix]] <- left_join(d, y, by = by, copy = copy, suffix = suffix)
  }
  x
}

left_join_u <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  left_join.bbl(x, matrix = "u", y = y, by = by, copy = copy, suffix = suffix)
}

left_join_v <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  left_join.bbl(x, matrix = "v", y = y, by = by, copy = copy, suffix = suffix)
}
