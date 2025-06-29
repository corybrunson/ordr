#' @title Format a tbl_ord for printing
#'
#' @description These methods of [base::format()] and [base::print()] render a
#'   (usually more) tidy readout of a [tbl_ord] that is consistent across all
#'   original ordination classes.
#'
#' @details
#'
#' The `format` and `print` methods for class 'tbl_ord' are adapted from those
#' for class '[tbl_df][tibble::tbl_df]' and for class 'tbl_graph' from the
#' **tidygraph** package.
#'
#' **Note:** The `format()` function is tedius but cannot be easily modularized
#' without invoking [recoverers], [annotation], and [augmentation] multiple
#' times, thereby significantly reducing performance.
#' 

#' @name format
#' @importFrom rlang "%||%"
#' @param x A [tbl_ord].
#' @param n Number(s) of rows to show from each matrix factor, handled as by
#'   [tibble::format.tbl()]. If length 1, will apply to both matrix factors.
#'   To pass `NULL` to only one factor, be sure to pass as a list, e.g. `n =
#'   list(6, NULL)`.
#' @inheritParams tibble::format.tbl
#' @param ... Additional arguments.
#' @return The `format()` method returns a vector of strings that are more
#'   elegantly printed by the `print()` method, which itself returns the tbl_ord
#'   invisibly.
#' @example inst/examples/ex-ord-format.r

#' @rdname format
#' @export
format.tbl_ord <- function(
  x, width = NULL, ..., n = NULL,
  max_extra_cols = NULL, max_footer_lines = NULL
) {
  
  # raw components and parameters
  dims <- get_factor(x, .matrix = "dims")
  n_dims <- sapply(dims, nrow)
  coord <- get_coord(x)
  rk <- length(coord)
  dims_ann <- mapply(
    bind_cols,
    annotation_factor(x, .matrix = "dims"),
    #recover_aug_factor(x, .matrix = "dims"),
    SIMPLIFY = FALSE
  )
  names(dims_ann) <- c("rows", "cols")
  n_ann <- sapply(dims_ann, ncol)
  
  # pre-process parameters
  if (is.null(n)) n <- list(NULL)
  if (length(n) == 1L) n <- rep(n, 2L)
  n <- n[seq(2L)]
  n <- ifelse(
    vapply(n, is.null, FALSE),
    ifelse(
      n_dims > tbl_ord_opt("print_max"),
      tbl_ord_opt("print_min"),
      n_dims
    ),
    n
  )
  n <- unlist(n)
  width <- width %||% tbl_ord_opt("width") %||% getOption("width")
  
  # headers!
  prev_class <- setdiff(class(x), "tbl_ord")[1]
  tbl_ord_header <- paste0(
    "# A tbl_ord",
    if (!is.null(prev_class) && prev_class != "list") {
      paste0(" of class '", prev_class, "'")
    },
    ": (", n_dims[1], " x ", rk, ") x (", n_dims[2], " x ", rk, ")'"
  )
  coord_header <- paste0(
    "# ", rk,
    " coordinate", if(rk == 1) "" else "s",
    ": ",
    print_reps(coord)
  )
  supp_header <- if (! is.null(attr(x, "rows_supplement")) |
                     ! is.null(attr(x, "rows_supplement"))) {
    paste0(
      "# ",
      if (! is.null(attr(x, "rows_supplement"))) {
        paste0(nrow(attr(x, "rows_supplement")), " supplementary rows")
      },
      if (! is.null(attr(x, "rows_supplement")) &
          ! is.null(attr(x, "cols_supplement"))) {
        " and "
      },
      if (! is.null(attr(x, "cols_supplement"))) {
        paste0(nrow(attr(x, "cols_supplement")), " supplementary columns")
      }
    )
  } else NULL
  x_inertia <- get_conference(x)
  inertia_name <- function(p) {
    if (p == 0) return("standard")
    if (p == 1) return("principal")
    if (p == 0.5) return("symmetric")
    paste0(round(100 * p, digits = 0L), "% inertia")
  }
  dims_inertia <- if (is.null(x_inertia)) NULL else {
    paste0(" (", vapply(x_inertia, inertia_name, ""), ")")
  }
  dims_headers <- paste0(
    "# ", c("Rows", "Columns"),
    dims_inertia,
    ": [ ", n_dims, " x ", rk, " | ", n_ann, " ]"
  )
  names(dims_headers) <- c("rows", "cols")
  
  # format rows and columns separately
  # (should format together, then split, in order to sync coordinates)
  fmt_coord_rows <- format(
    as_tibble(dims$rows)[, seq(min(rk, 3)), drop = FALSE],
    n = n[[1L]], width = width / 2
  )
  fmt_coord_cols <- format(
    as_tibble(dims$cols)[, seq(min(rk, 3)), drop = FALSE],
    n = n[[2L]], width = width / 2
  )
  fmt_coord <- list(
    rows = unname(c(
      dims_headers["rows"],
      fmt_coord_rows[2],
      stringr::str_pad("", nchar(fmt_coord_rows[2])),
      fmt_coord_rows[seq(4, length(fmt_coord_rows))]
    )),
    cols = unname(c(
      dims_headers["cols"],
      fmt_coord_cols[2],
      stringr::str_pad("", nchar(fmt_coord_cols[2])),
      fmt_coord_cols[seq(4, length(fmt_coord_cols))]
    ))
  )
  
  # footers?
  dims_footers <- n_dims - n > 0
  fmt_ann <- lapply(seq(2), function(i) {
    if (ncol(dims_ann[[i]]) == 0) return("")
    # dodge `format.pillar_shaft_decimal()` errors
    wid_try <- (width - 7) / 2
    #wid_try <- width - 7
    fmt_try <- try(
      c("", format(dims_ann[[i]], n = n[[i]], width = wid_try)[-1]),
      silent = TRUE
    )
    while (class(fmt_try) == "try-error") {
      wid_try <- wid_try - 1
      fmt_try <- c("", format(dims_ann[[i]], n = n[[i]], width = wid_try)[-1])
    }
    fmt_try
  })
  names(fmt_ann) <- c("rows", "cols")
  # -+- allow additional rows/variables statement to fill horizontal space -+-
  
  # separate coordinates from annotations
  seps <- if (rk > 3) c("    ", " ...") else c("", "")
  fmt_seps <- mapply(
    function(x, y) {
      sep_dots_rows <- ceiling(c(2, (y - 2) / 2 + 2))
      c(paste(rep(" ", times = max(0, x)), collapse = ""),
        paste0(ifelse(seq(2, y) %in% sep_dots_rows, seps[2], seps[1]), " | "))
    },
    x = 3 + nchar(seps) -
      sapply(fmt_coord, function(z) nchar(z[1])),
    y = sapply(fmt_coord, length),
    SIMPLIFY = FALSE
  )
  
  # paste together, with attention to footers
  for (i in seq(2L)) {
    if (dims_footers[i]) {
      fmt_coord[[i]] <- c(fmt_coord[[i]], "")
      fmt_seps[[i]] <- c(fmt_seps[[i]], "")
    }
  }
  # add blank lines if necessary to allow footers
  for (i in seq(2)) {
    len_coord <- length(fmt_coord[[i]])
    len_seps <- length(fmt_seps[[i]])
    len_ann <- length(fmt_ann[[i]])
    stopifnot(len_coord == len_seps)
    if (len_coord < len_ann) {
      fmt_coord[[i]][seq(len_coord + 1L, len_ann)] <- ""
      fmt_seps[[i]][seq(len_seps + 1L, len_ann)] <- ""
    }
  }
  fmt_dims <- mapply(
    paste0,
    fmt_coord, fmt_seps, fmt_ann,
    SIMPLIFY = FALSE
  )
  
  c(
    tbl_ord_header,
    coord_header,
    supp_header,
    "# ", fmt_dims[[1L]], "# ", fmt_dims[[2L]]
  )
}

#' @rdname format
#' @export
print.tbl_ord <- function(
  x, width = NULL, ..., n = NULL,
  max_extra_cols = NULL, max_footer_lines = NULL
) {
  fmt <- format(
    x, width = width, ..., n = n,
    max_extra_cols = max_extra_cols, max_footer_lines = max_footer_lines
  )
  cat(paste(fmt, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#`%||%` <- rlang::`%||%`

# this trick is borrowed from *tibble*
op.tbl_ord <- list(
  tbl_ord.print_max = 10L,
  tbl_ord.print_min = 5L,
  tbl_ord.width = NULL,
  tbl_ord.max_extra_cols = 50L
)

tbl_ord_opt <- function(x) {
  x_tbl_ord <- paste0("tbl_ord.", x)
  res <- getOption(x_tbl_ord)
  if (!is.null(res)) {
    return(res)
  }
  
  x_tibble <- paste0("tibble.", x)
  res <- getOption(x_tibble)
  if (!is.null(res)) {
    return(as.integer(res / 2))
  }
  
  op.tbl_ord[[x_tbl_ord]]
}

print_reps <- function(x) {
  x <- as.character(x)
  switch(
    min(length(x), 4),
    "1" = x,
    "2" = paste(x, collapse = " and "),
    "3" = paste(x, collapse = ", "),
    "4" = paste0(paste(x[seq(2)], collapse = ", "), ", ..., ", x[length(x)])
  )
}
