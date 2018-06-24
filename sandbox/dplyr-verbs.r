
ensure_tibble_annot <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  which <- paste0(.matrix, "_annot")
  if (is.null(attr(x, which, exact = TRUE))) {
    attr(x, which) <- as_tibble(matrix(
      NA,
      nrow = nrow(get_factor(x, .matrix)),
      ncol = 0
    ))
  } else if (!is.list(attr(x, which, exact = TRUE))) {
    stop("Attribute '", which, "' of `", deparse(substitute(x)),
         "` is not a list.")
  }
  x
}

non_transformable_dots <- function(x, .matrix, ...) {
  vars <- select_vars(names(fortify(x)), !!!quos(...))
  # don't transform coordinates
  if (any(vars %in% get_coord(x))) {
    var <- vars[which(vars %in% get_coord(x))][1]
    stop("The coordinate '", var, "' of `", deparse(substitute(x)),
         "` cannot be transformed.")
  }
  # don't transform augmentation
  if (any(vars %in% names(augment(x, .matrix = .matrix)))) {
    var <- vars[which(vars %in% names(augment(x, .matrix = .matrix)))][1]
    stop("The augmented variable '", var, "' of `", deparse(substitute(x)),
         "` cannot be transformed.")
  }
}

pull_u <- function(.data, var = -1) {
  stopifnot(inherits(.data, "bbl"))
  pull(fortify_u(.data), !!enquo(var))
}
pull_v <- function(.data, var = -1) {
  stopifnot(inherits(.data, "bbl"))
  pull(fortify_v(.data), !!enquo(var))
}

rename.bbl <- function(.data, ..., .matrix = "uv") {
  .matrix <- match_factor(.matrix)
  # don't transform coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data)$.name)) {
    stop("The coordinates of `.data` are protected.")
  }
  # don't transform augmentation
  if (any(names(quos(...)) %in% names(augment(.data, .matrix = .matrix)))) {
    stop("Cannot rename bibble augmentation.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      attr(.data, paste0(.m, "_annot")) <-
        rename(attr(.data, paste0(.m, "_annot")), ...)
    }
  } else {
    attr(.data, paste0(.matrix, "_annot")) <-
      rename(attr(.data, paste0(.matrix, "_annot")), ...)
  }
  .data
}
rename_u <- function(.data, ...) {
  stopifnot(inherits(.data, "bbl"))
  rename.bbl(.data, ..., .matrix = "u")
}
rename_v <- function(.data, ...) {
  stopifnot(inherits(.data, "bbl"))
  rename.bbl(.data, ..., .matrix = "v")
}

select.bbl <- function(.data, ..., .matrix = "uv") {
  .matrix <- match_factor(.matrix)
  # don't transform coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data)$.name)) {
    stop("The coordinates of `.data` are protected.")
  }
  # don't transform augmentation
  if (any(names(quos(...)) %in% names(augment(.data, .matrix = .matrix)))) {
    stop("Cannot rename bibble augmentation.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      .data[[.m]] <- bind_cols(
        factor_uv(.data, .m),
        select(attr_uv(.data, .m), ...)
      )
    }
  } else {
    .data[[.matrix]] <- bind_cols(
      factor_uv(.data, .matrix),
      select(attr_uv(.data, .matrix), ...)
    )
  }
  .data
}
select_u <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  select.bbl(.data, ..., .matrix = "u")
}
select_v <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  select.bbl(.data, ..., .matrix = "v")
}

mutate.bbl <- function(.data, ..., .matrix = "uv") {
  .data <- to_bibble(.data)
  .matrix <- match_factor(.matrix)
  # don't allow mutations of coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data)$.name)) {
    stop("The coordinates of `.data` are immutable.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      .data[[.m]] <- bind_cols(
        factor_uv(.data, .m),
        mutate(attr_uv(.data, .m), ...)
      )
    }
  } else {
    .data[[.matrix]] <- bind_cols(
      factor_uv(.data, .matrix),
      mutate(attr_uv(.data, .matrix), ...)
    )
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

transmute.bbl <- function(.data, ..., .matrix = "uv") {
  .data <- to_bibble(.data)
  .matrix <- match_factor(.matrix)
  # don't allow mutations of coordinates
  if (any(names(quos(...)) %in% get_coordinates(.data)$.name)) {
    stop("The coordinates of `.data` are immutable.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      .data[[.m]] <- bind_cols(
        factor_uv(.data, .m),
        transmute(attr_uv(.data, .m), ...)
      )
    }
  } else {
    .data[[.matrix]] <- bind_cols(
      factor_uv(.data, .matrix),
      transmute(attr_uv(.data, .matrix), ...)
    )
  }
  .data
}
transmute_u <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  transmute.bbl(.data, ..., .matrix = "u")
}
transmute_v <- function(.data, ...) {
  stopifnot(class(.data)[1] == "bbl")
  transmute.bbl(.data, ..., .matrix = "v")
}

bind_cols.bbl <- function(.data, ..., .matrix = "uv") {
  .data <- to_bibble(.data)
  .matrix <- match_factor(.matrix)
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
  x, y, .matrix = "uv", by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  .matrix <- match_factor(.matrix)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      x[[.m]] <- inner_join(
        get_uv(x, .m),
        y, by = by, copy = copy, suffix = suffix
      )
      if (nrow(x[[.m]]) == 0)
        stop("No matching values for `by` variables in matrix `", .m, "`.")
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
  x, y, .matrix = "uv", by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  .matrix <- match_factor(.matrix)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      x[[.m]] <- left_join(
        get_uv(x, .m),
        y, by = by, copy = copy, suffix = suffix
      )
      if (nrow(x[[.m]]) == 0)
        stop("No matching values for `by` variables in matrix `", .m, "`.")
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

right_join.bbl <- function(
  x, y, .matrix = "uv", by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  x <- to_bibble(x)
  .matrix <- match_factor(.matrix)
  # don't allow joins by or with coordinates
  if (any(names(y) %in% get_coordinates(x))) {
    stop("Object `y` shares column names with the coordinates of `x`.")
  }
  if (.matrix == "uv") {
    for (.m in c("u", "v")) {
      x[[.m]] <- right_join(
        get_uv(x, .m),
        y, by = by, copy = copy, suffix = suffix
      )
      if (nrow(x[[.m]]) == 0)
        stop("No matching values for `by` variables in matrix `", .m, "`.")
    }
  } else {
    d <- get_uv(x, .matrix)
    x[[.matrix]] <- right_join(d, y, by = by, copy = copy, suffix = suffix)
  }
  x
}
right_join_u <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  right_join.bbl(x, y = y, .matrix = "u", by = by, copy = copy, suffix = suffix)
}
right_join_v <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  stopifnot(class(x)[1] == "bbl")
  right_join.bbl(x, y = y, .matrix = "v", by = by, copy = copy, suffix = suffix)
}
