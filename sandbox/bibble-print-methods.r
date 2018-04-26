devtools::load_all()

data(country_differences, country_attributes)
# format both tibbles as matrices
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

# multidimensional scaling setup
(m <- cmdscale(x1, k = 2))
(a <- as_bibble(m))
(b <- bibble(m))

# format.bbl

format.bbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  
  x <- a
  n <- NULL
  width <- NULL
  n_extra <- NULL
  
  # dimensional parameters
  rows <- sapply(get_uv(x), nrow)
  ncoord <- nrow(get_coordinates(x))
  if (is.null(n)) {
    n <- ifelse(
      rows > op.bibble$bibble.print_max,
      op.bibble$bibble.print_min,
      rows
    )
  }
  width <- width %||% op.bibble$bibble.width %||% getOption("width")
  widths <- floor(
    (width - 3) * (c(0, 1) + c(mat = 1, ann = -1) * op.bibble$bibble.coord_prop)
  )
  n_extra <- rep(n_extra %||% op.bibble$bibble.max_extra_cols, length.out = 2)
  
  header <- bbl_sum(x)
  
  format_coord <- lapply(c("u", "v"), function(matrix) {
    fmt <- format(as_tibble(factor_uv(x, matrix)), width = widths[1])[-1]
    if (grepl("^#", fmt[length(fmt)])) {
      footer <- fmt[length(fmt)]
      fmt <- fmt[-length(fmt)]
    }
    fmt
  })
  
  format_annot
  
  paste(
    format(as_tibble(factor_u(x))),
    format(select(get_u(x), -one_of(get_coordinates(x)$.name))),
    sep = " | "
  )[-1]
  lapply(c("u", "v"), function(matrix) {
    paste(
      format(as_tibble(factor_uv(x, matrix))),
      format(select(get_uv(x, matrix), -one_of(get_coordinates(x)$.name))),
      sep = " | "
    )[-1]
  })
}

x <- a
n <- NULL
width <- NULL
n_extra <- NULL

rows <- sapply(get_uv(x), nrow)
ncoord <- nrow(get_coordinates(x))
if (is.null(n)) {
  n <- ifelse(
    rows > op.bibble$bibble.print_max,
    op.bibble$bibble.print_min,
    rows
  )
}
n_extra <- rep(n_extra %||% op.bibble$bibble.max_extra_cols, length.out = 2)
df <- lapply(lapply(
  mapply(head, x = get_uv(x), n = n, SIMPLIFY = FALSE),
  as.data.frame
), select, get_coordinates(x)$.name, everything())
shrunk <- mapply(shrink_factor, df, rows, n, SIMPLIFY = FALSE)
factor_desc <- tbl_sum(x)
trunc_info <- lapply(1:2, function(i) list(
  width = width, rows_total = rows[[i]],
  rows_min = nrow(df[[i]]), n_extra = n_extra[i],
  summary = factor_desc[i]
))
structure(list(shrunk, trunc_info), class = "trunc_mat_bbl")

# print.bbl

print.bbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  fmt <- format(x, ..., n = n, width = width, n_extra = n_extra)
  cat(paste0(fmt, collapse = "\n"), sep = "")
  invisible(x)
}
