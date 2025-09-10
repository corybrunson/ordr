
.ord_factors <- c(
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

.ord_elements <- c("active", "score", "structure")

match_factor <- function(x) {
  x <- match.arg(tolower(x), names(.ord_factors))
  unname(.ord_factors[x])
}
switch_inertia <- function(x) {
  x <- match.arg(tolower(x), names(.ord_factors))
  switch(.ord_factors[x], rows = c(1, 0), cols = c(0, 1), dims = c(.5, .5))
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

factor_coord <- function(x) {
  if (any(duplicated(x))) stop("Duplicated coordinates detected.")
  factor(x, levels = x)
}

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
  names(data)[ord_cols]
}

# restrict to specified elements
setup_elts_data <- function(data, params) {
  
  if (is.null(params$elements))
    # default to active elements
    params$elements <- "active"
  else
    # match `elements` to a list of recognized options (excluding `"all"`)
    params$elements <- match.arg(params$elements, .ord_elements)
  
  # subset accordingly
  data <- data[data$.element == params$elements, , drop = FALSE]
  
  # print note if both `elements` and `subset` are passed
  if (! is.null(params$subset)) {
    rlang::inform(
      paste0(
        "`subset` will be applied after data are restricted to ",
        params$elements, " elements."
      ),
      .frequency = "once", .frequency_id = "setup_elts_data-subset"
    )
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

# restrict to a matrix factor
setup_rows_data <- function(data, params) {
  
  data <-
    data[data$.matrix == "rows", -match(".matrix", names(data)), drop = FALSE]
  
  setup_elts_data(data, params)
}
setup_cols_data <- function(data, params) {
  
  data <-
    data[data$.matrix == "cols", -match(".matrix", names(data)), drop = FALSE]
  
  setup_elts_data(data, params)
}

# restrict to a matrix factor and to the first two coordinates
# (for stat layers that only accept 'x' and 'y')
setup_rows_xy_data <- function(data, params) {
  data <- setup_rows_data(data, params)
  
  ord_cols <- get_ord_aes(data)
  # if necessary, restore 'x' and 'y' from first and second coordinates
  if (any(is.na(match(c("x", "y"), ord_cols)))) {
    xy_cols <- match(c("..coord1", "..coord2"), names(data))
    # names(data)[xy_cols] <- c("x", "y")
    data$x <- data[[xy_cols[[1L]]]]
    data$y <- data[[xy_cols[[2L]]]]
  }
  
  data
}
setup_cols_xy_data <- function(data, params) {
  data <- setup_cols_data(data, params)
  
  ord_cols <- get_ord_aes(data)
  # if necessary, restore 'x' and 'y' from first and second coordinates
  if (any(is.na(match(c("x", "y"), ord_cols)))) {
    xy_cols <- match(c("..coord1", "..coord2"), names(data))
    # names(data)[xy_cols] <- c("x", "y")
    data$x <- data[[xy_cols[[1L]]]]
    data$y <- data[[xy_cols[[2L]]]]
  }
  
  data
}

setup_referent_params <- function(self, data, params) {
  
  if (is.null(params$referent)) {
    # default null `referent` to other matrix factor
    .matrix <- tolower(gsub("^Stat(Rows|Cols).*$", "\\1", class(self)[[1L]]))
    stopifnot(.matrix %in% c("rows", "cols"))
    setup_factor_xy_data <- switch(
      .matrix,
      rows = setup_cols_xy_data,
      cols = setup_rows_xy_data
    )
    params$referent <- setup_factor_xy_data(
      data,
      list(elements = params$ref_elements, subset = params$ref_subset)
    )
  } else {
    # continue with parent parameter setup
    params$referent <- as.data.frame(params$referent)
    params <- ggproto_parent(StatReferent, self)$setup_params(data, params)
  }
  
  params
}

ord_formals <- function(`_class`, method) {
  fun <- environment(`_class`[[method]])[[method]]
  formals(fun) <- c(formals(fun), list(subset = NULL, elements = "active"))
  fun
}
