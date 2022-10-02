
#' @importFrom utils getFromNamespace

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

as_tbl_ord_default <- function(x) {
  class(x) <- c("tbl_ord", class(x))
  x
}

tbl_ord_factors <- c(
  rows = "rows", columns = "cols", cols = "cols", dims = "dims",
  f = "rows", g = "cols", fg = "dims",
  u = "rows", v = "cols", uv = "dims",
  left = "rows", right = "cols",
  # cases = "rows", variables = "cols",
  # subjects = "rows", measures = "cols",
  # scores = "rows", loadings = "cols",
  rowprincipal = "rows", colprincipal = "cols", columnprincipal = "cols",
  both = "dims", symmetric = "dims"
)
match_factor <- function(x) {
  x <- match.arg(tolower(x), names(tbl_ord_factors))
  unname(tbl_ord_factors[x])
}
switch_inertia <- function(x) {
  x <- match.arg(tolower(x), names(tbl_ord_factors))
  switch(tbl_ord_factors[x], rows = c(1, 0), cols = c(0, 1), dims = c(.5, .5))
}

method_classes <- function(generic.function) {
  stringr::str_replace(
    stringr::str_subset(
      rownames(attr(utils::methods(generic.function), "info")),
      paste0("^", generic.function, "\\.")
    ),
    paste0("^", generic.function, "\\."), ""
  )
}

tibble_pole <- function(nrow) {
  as_tibble(matrix(nrow = nrow, ncol = 0))
}

factor_coord <- function(x) {
  if (any(duplicated(x))) stop("Duplicated coordinates detected.")
  factor(x, levels = x)
}

# `ggbiplot` utilities

matrix_stat <- function(.matrix, stat) {
  .matrix <- match_factor(.matrix)
  if (stat == "identity") return(.matrix)
  stringr::str_c(.matrix, stat, sep = "_")
}
rows_stat <- function(stat) matrix_stat("rows", stat)
cols_stat <- function(stat) matrix_stat("cols", stat)

# get ordination coordinate mapping from a data frame in a stat layer:
# `ord_aes()` if specified, otherwise 'x' and 'y'
get_ord_aes <- function(data) {
  ord_cols <- grep("^\\.\\.coord[0-9]+$", names(data))
  if (length(ord_cols) == 0) ord_cols <- match(c("x", "y"), names(data))
  ord_cols
}

# restrict to a matrix factor
setup_rows_data <- function(data, params) {
  
  data <-
    data[data$.matrix == "rows", -match(".matrix", names(data)), drop = FALSE]
  
  # if specified and possible, restrict to active or supplementary elements
  if (! is.null(params$elements) && ".element" %in% names(data)) {
    # ensure that `elements` is a character singleton
    stopifnot(
      is.character(params$elements),
      length(params$elements) == 1L
    )
    # subset accordingly
    data <- if ("all" %in% params$elements) {
      data
    } else {
      data[data$.element == params$elements, , drop = FALSE]
    }
    # print note if both `elements` and `subset` are passed
    if (! is.null(params$subset)) {
      message("`subset` will be applied after data are restricted to ",
              params$elements, " elements.")
    }
  }
  
  # by default, render elements for all rows
  if (! is.null(params$subset)) {
    if (is.numeric(params$subset)) {
      data <- data[params$subset, , drop = FALSE]
    } else if (is.character(params$subset)) {
      warning("`subset` cannot yet accept names.")
    } else {
      warning("`subset` of unrecognized type will be ignored.")
    }
  }
  
  data
}
setup_cols_data <- function(data, params) {
  
  data <-
    data[data$.matrix == "cols", -match(".matrix", names(data)), drop = FALSE]
  
  # if specified and possible, restrict to active or supplementary elements
  if (! is.null(params$elements) && ".element" %in% names(data)) {
    # ensure that `elements` is a character singleton
    stopifnot(
      is.character(params$elements),
      length(params$elements) == 1L
    )
    # subset accordingly
    data <- if ("all" %in% params$elements) {
      data
    } else {
      data[data$.element == params$elements, , drop = FALSE]
    }
    # print note if both `elements` and `subset` are passed
    if (! is.null(params$subset)) {
      message("`subset` will be applied after data are restricted to ",
              params$elements, " elements.")
    }
  }
  
  # by default, render elements for all columns
  if (! is.null(params$subset)) {
    if (is.numeric(params$subset)) {
      data <- data[params$subset, , drop = FALSE]
    } else if (is.character(params$subset)) {
      warning("`subset` cannot yet accept names.")
    } else {
      warning("`subset` of unrecognized type will be ignored.")
    }
  }
  
  data
}

# restrict to a matrix factor and to the first two coordinates
# (for stat layers that only accept 'x' and 'y')
setup_rows_xy_data <- function(data, params) {
  data <- setup_rows_data(data, params)
  
  ord_cols <- get_ord_aes(data)
  # if necessary, restore 'x' and 'y' from first and second coordinates
  if (any(is.na(match(c("x", "y"), names(data)[ord_cols])))) {
    xy_cols <- match(c("..coord1", "..coord2"), names(data)[ord_cols])
    names(data)[xy_cols] <- c("x", "y")
  }
  
  data
}
setup_cols_xy_data <- function(data, params) {
  data <- setup_cols_data(data, params)
  
  ord_cols <- get_ord_aes(data)
  # if necessary, restore 'x' and 'y' from first and second coordinates
  if (any(is.na(match(c("x", "y"), names(data)[ord_cols])))) {
    xy_cols <- match(c("..coord1", "..coord2"), names(data)[ord_cols])
    names(data)[xy_cols] <- c("x", "y")
  }
  
  data
}

is_const <- function(x) length(unique(x)) == 1L
