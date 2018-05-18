
pull_u <- function(.data, var = -1) {
  stopifnot(class(.data)[1] == "bbl")
  pull(get_uv(.data, .matrix = "u"), !!enquo(var))
}
pull_v <- function(.data, var = -1) {
  stopifnot(class(.data)[1] == "bbl")
  pull(get_uv(.data, .matrix = "v"), !!enquo(var))
}

rename.bbl <- function(.data, ..., .matrix = NULL) {
  .data <- to_bibble(.data)
  if (!is.null(.matrix)) {
    .matrix <- match_factor(.matrix)
    if (.matrix == "uv") {
      for (.m in c("u", "v")) {
        .data[[.m]] <- bind_cols(
          factor_uv(.data, .m),
          rename(attr_uv(.data, .m), ...)
        )
      }
    } else {
      .data[[.matrix]] <- bind_cols(
        factor_uv(.data, .matrix),
        rename(attr_uv(.data, .matrix), ...)
      )
    }
  } else {
    # if `.matrix` is `NULL`, try renaming each field in both factors
    for (.m in c("u", "v")) {
      dots <- quos(...)
      d <- attr_uv(.data, .m)
      for (i in seq_along(dots)) {
        dot <- dots[i]
        d_tmp <- try(rename(d, !!!dot))
        if (class(d_tmp)[1] == "try-error") next
        d <- d_tmp
      }
      .data[[.m]] <- d
    }
  }
  .data
}
rename.bbl_alt <- function(.data, ..., .matrix = NULL) {
  .data <- to_bibble(.data)
  # don't allow renamings of coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data))) {
    stop("The coordinate names of `.data` are protected.")
  }
  # if `.matrix` is `NULL`, try renaming fields in both factors
  if (is.null(.matrix)) {
    for (.matrix in c("u", "v")) {
      dots <- quos(...)
      d <- get_uv(.data, .matrix = .matrix)
      for (i in seq_along(dots)) {
        dot <- dots[i]
        d_tmp <- try(rename(d, !!!dot))
        if (class(d_tmp)[1] == "try-error") next
        d <- d_tmp
      }
      .data[[.matrix]] <- d
    }
  } else if (.matrix == "uv") {
    for (.matrix in c("u", "v")) {
      d <- get_uv(.data, .matrix)
      .data[[.matrix]] <- rename(d, ...)
    }
  } else {
    d <- get_uv(.data, .matrix)
    .data[[.matrix]] <- rename(d, ...)
  }
  .data
}
rename_u <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  rename.bbl(.data, ..., .matrix = "u")
}
rename_v <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  rename.bbl(.data, ..., .matrix = "v")
}

mutate.bbl <- function(.data, ..., .matrix = NULL) {
  .data <- to_bibble(.data)
  # don't allow mutations of coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data))) {
    stop("The coordinates of `.data` are immutable.")
  }
  # if `.matrix` is `NULL`, try each mutation in both factors
  if (is.null(.matrix)) {
    for (.matrix in c("u", "v")) {
      dots <- quos(...)
      d <- get_uv(.data, .matrix = .matrix)
      for (i in seq_along(dots)) {
        dot <- dots[i]
        d_tmp <- try(mutate(d, !!!dot))
        if (class(d_tmp)[1] == "try-error") next
        d <- d_tmp
      }
      .data[[.matrix]] <- d
    }
  } else if (.matrix == "uv") {
    for (.matrix in c("u", "v")) {
      d <- get_uv(.data, .matrix)
      .data[[.matrix]] <- mutate(d, ...)
    }
  } else {
    d <- get_uv(.data, .matrix)
    .data[[.matrix]] <- mutate(d, ...)
  }
  .data
}
mutate_u <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  mutate.bbl(.data, ..., .matrix = "u")
}
mutate_v <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  mutate.bbl(.data, ..., .matrix = "v")
}

bind_cols.bbl <- function(.data, ..., .matrix = NULL) {
  .data <- to_bibble(.data)
  if (is.null(.matrix)) {
    warning("`NULL` method not yet implemented; using `.matrix = 'uv'`.")
    .matrix <- "uv"
  }
  if (.matrix == "uv") {
    for (.matrix in c("u", "v")) {
      d <- get_uv(.data, .matrix)
      .data[[.matrix]] <- bind_cols(d, ...)
    }
  } else {
    d <- get_uv(.data, .matrix)
    .data[[.matrix]] <- bind_cols(d, ...)
  }
  .data
}
bind_cols_u <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  bind_cols.bbl(.data, ..., .matrix = "u")
}
bind_cols_v <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  bind_cols.bbl(.data, ..., .matrix = "v")
}

inner_join.bbl <- function(
  x, y, .matrix = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (is.null(.matrix)) {
    warning("`NULL` method not yet implemented; using `.matrix = 'uv'`.")
    .matrix <- "uv"
  }
  if (.matrix == "uv") {
    for (.matrix in c("u", "v")) {
      d <- get_uv(x, .matrix)
      x[[.matrix]] <- inner_join(d, y, by = by, copy = copy, suffix = suffix)
      if (nrow(x[[.matrix]]) == 0)
        stop("No matching values for `by` variables in matrix `", .matrix, "`.")
    }
  } else {
    d <- get_uv(x, .matrix)
    x[[.matrix]] <- inner_join(d, y, by = by, copy = copy, suffix = suffix)
  }
  x
}
inner_join_u <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  inner_join.bbl(x, y = y, .matrix = "u", by = by, copy = copy, suffix = suffix)
}
inner_join_v <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  inner_join.bbl(x, y = y, .matrix = "v", by = by, copy = copy, suffix = suffix)
}

left_join.bbl <- function(
  x, y, .matrix = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (is.null(.matrix)) {
    .matrix <- ""
    if (length(intersect(names(x$u), names(y))) > 0)
      .matrix <- paste0(.matrix, "u")
    if (length(intersect(names(x$v), names(y))) > 0)
      .matrix <- paste0(.matrix, "v")
    if (.matrix == "")
      stop("`by` required, because the data sources have no common variables.")
  }
  if (.matrix == "uv") {
    for (.matrix in c("u", "v")) {
      d <- get_uv(x, .matrix)
      x[[.matrix]] <- left_join(d, y, by = by, copy = copy, suffix = suffix)
      if (nrow(x[[.matrix]]) == 0)
        stop("No matching values for `by` variables in matrix `", .matrix, "`.")
    }
  } else {
    d <- get_uv(x, .matrix)
    x[[.matrix]] <- left_join(d, y, by = by, copy = copy, suffix = suffix)
  }
  x
}
left_join_u <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  left_join.bbl(x, y = y, .matrix = "u", by = by, copy = copy, suffix = suffix)
}
left_join_v <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  left_join.bbl(x, y = y, .matrix = "v", by = by, copy = copy, suffix = suffix)
}
