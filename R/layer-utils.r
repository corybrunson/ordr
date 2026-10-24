
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

.ord_elements <- c("active", "score", "structure", "pinv_weight")

snake_class <- getFromNamespace("snake_class", "ggplot2")

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

# TODO: Return a ggproto rather than a character string?
matrix_stat <- function(.matrix, stat) {
  .matrix <- match_factor(.matrix)
  stopifnot(is.character(stat) || inherits(stat, c("Stat", "ggproto")))
  if (! is.character(stat)) {
    if (identical(stat, Stat))
      stop("`Stat`, rather than a child class, was passed to `matrix_stat()`.")
    stat <- gsub("^stat\\_", "", snake_class(stat))
  }
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

# compute the squared norm of each axis across its mapped model coordinates
setup_axis_ss <- function(data) {
  ord_cols <- get_ord_aes(data)
  if (length(ord_cols) < 2L) return(data)
  data$axis.ss <- rowSums(data[, ord_cols, drop = FALSE]^2)
  data
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

  conf_rows <- NA_real_
  conf_cols <- NA_real_
  if (".conference" %in% names(data)) {
    cr <- unique(data$.conference[data$.matrix == "rows"])
    cc <- unique(data$.conference[data$.matrix == "cols"])
    if (length(cr) == 1L) conf_rows <- cr[[1L]]
    if (length(cc) == 1L) conf_cols <- cc[[1L]]
  }

  warn_predictive_conference(conf_rows, conf_cols, "rows", params)

  data <-
    data[data$.matrix == "rows", setdiff(names(data), ".matrix"), drop = FALSE]

  data <- setup_elts_data(data, params)
  setup_axis_ss(data)
}
setup_cols_data <- function(data, params) {

  conf_rows <- NA_real_
  conf_cols <- NA_real_
  if (".conference" %in% names(data)) {
    cr <- unique(data$.conference[data$.matrix == "rows"])
    cc <- unique(data$.conference[data$.matrix == "cols"])
    if (length(cr) == 1L) conf_rows <- cr[[1L]]
    if (length(cc) == 1L) conf_cols <- cc[[1L]]
  }

  warn_predictive_conference(conf_rows, conf_cols, "cols", params)

  data <-
    data[data$.matrix == "cols", setdiff(names(data), ".matrix"), drop = FALSE]

  data <- setup_elts_data(data, params)
  setup_axis_ss(data)
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

warn_predictive_conference <- function(
    conf_rows, conf_cols, axis_factor, params
) {
  if (is.null(params) || ! isTRUE(params$axis.type == "predictive")) {
    return(invisible(NULL))
  }

  conf_axis <- if (identical(axis_factor, "rows")) conf_rows else conf_cols

  axis_is_standard <- is.numeric(conf_axis) && length(conf_axis) == 1L &&
    ! is.na(conf_axis) && abs(conf_axis - 0) < 1e-12

  pair_balanced <- FALSE
  if (is.numeric(conf_rows) && is.numeric(conf_cols) &&
      length(conf_rows) == 1L && length(conf_cols) == 1L &&
      ! is.na(conf_rows) && ! is.na(conf_cols)) {
    pair_balanced <- (abs(conf_rows - 0) < 1e-12 && abs(conf_cols - 1) < 1e-12)
  }

  causes <- character()
  if (! axis_is_standard) {
    causes <- c(causes, paste0(axis_factor, " are non-standard"))
  }
  if (! pair_balanced) {
    causes <- c(causes, "inertia is not balanced")
  }

  if (length(causes) == 0L) return(invisible(NULL))

  msg <- paste0(
    "Predictive axes assume standard coordinates and balanced inertia; ",
    paste(causes, collapse = " and ")
  )

  if (requireNamespace("rlang", quietly = TRUE)) {
    rlang::warn(
      msg,
      .frequency = "once",
      .frequency_id = paste0("ordr-predictive-conference-", axis_factor,
                             "-", paste(gsub("[^A-Za-z0-9]+", "-", causes), collapse = "-"))
    )
  } else {
    warning(msg, call. = FALSE)
  }

  invisible(NULL)
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
