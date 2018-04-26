
# formatting utilities

op.bibble <- list(
  bibble.print_max = 9L,
  bibble.print_min = 4L,
  bibble.width = NULL,
  bibble.coord_prop = .6,
  bibble.max_extra_cols = 60
)

shrink_factor <- function(df, rows, n) {
  if (is.na(rows)) {
    needs_dots <- (nrow(df) >= n)
  } else {
    needs_dots <- (rows > n)
  }
  if (needs_dots) {
    rows_missing <- rows - n
  } else {
    rows_missing <- 0L
  }
  mcf <- pillar::colonnade(df, has_row_id = TRUE, needs_dots = needs_dots)
  list(mcf = mcf, rows_missing = rows_missing)
}

bbl_sum <- function(x) UseMethod("bbl_sum")
bbl_sum.bbl <- function(x) {
  prev_class <- setdiff(c(attr(x, "preclass"), class(x)), "bbl")[1]
  wrapped <- is.null(attr(x, "preclass"))
  biplot_descr <- if (wrapped) {
    paste0("# A bibble-wrapped '", prev_class, "' object")
  } else if (!is.null(prev_class) && prev_class != "list") {
    paste0("# A bibble from a '", prev_class, "' object")
  } else {
    paste0("# A bibble")
  }
  n_u <- nrow(get_u(x))
  n_v <- nrow(get_v(x))
  n_c <- nrow(get_coordinates(x))
  paste0(biplot_descr,
         ": ( ", n_u, " x ", n_c, " ) x ( ", n_v, " x ", n_c, " )'")
}
tbl_sum.bbl <- function(x) {
  d <- sapply(get_uv(x), dim)
  m <- sapply(factor_uv(x), dim)
  res <- paste0("[", d[1, ], " x ", m[2, ], " | ", d[2, ] - m[2, ], "]")
  names(res) <- c("U", "V")
  res
}
