# https://github.com/tidyverse/tibble/blob/master/R/tbl-df.r

# trunc_mat

x <- mtcars
n <- NULL
width <- NULL
n_extra <- NULL

rows <- nrow(x)

tibble:::tibble_opt("print_min")
rows
if (is_null(n)) {
  if (is.na(rows) || rows > tibble:::tibble_opt("print_max")) {
    n <- tibble:::tibble_opt("print_min")
  } else {
    n <- rows
  }
}

tibble:::tibble_opt("max_extra_cols")
n_extra <- n_extra %||% tibble:::tibble_opt("max_extra_cols")

as.data.frame(head(x, n))
df <- as.data.frame(head(x, n))

tibble:::has_rownames(x)
tibble:::shrink_mat(df, rows, n, star = tibble:::has_rownames(x))
shrunk <- tibble:::shrink_mat(df, rows, n, star = tibble:::has_rownames(x))

tbl_sum(x)
trunc_info <- list(
  width = width, rows_total = rows, rows_min = nrow(df),
  n_extra = n_extra, summary = tbl_sum(x)
)

c(paste0("trunc_mat_", class(x)), "trunc_mat")
structure(
  c(shrunk, trunc_info),
  class = c(paste0("trunc_mat_", class(x)), "trunc_mat")
)

# format.trunc_mat

x <- trunc_mat(mtcars)
width <- NULL

x$width
if (is.null(width)) {
  width <- x$width
}

tibble:::tibble_width(width)
width <- tibble:::tibble_width(width)

tibble:::format_header
tibble:::format_header(x)
named_header <- tibble:::format_header(x)

rlang:::names2
rlang:::names2(named_header)
if (all(rlang:::names2(named_header) == "")) {
  header <- named_header
} else {
  header <- paste0(tibble:::justify(
    paste0(rlang:::names2(named_header), ":"),
    right = FALSE, space = " "
  ), " ", named_header)
}
header

tibble:::format_comment(header, width = width)
comment <- tibble:::format_comment(header, width = width)

x$mcf
pillar::squeeze
pillar::squeeze(x$mcf, width = 48)
pillar::squeeze(x$mcf, width = width)
squeezed <- pillar::squeeze(x$mcf, width = width)

tibble:::format_body
mcf <- tibble:::format_body(squeezed)

tibble:::format_footer(x, squeezed)
tibble:::pre_dots(tibble:::format_footer(x, squeezed))
tibble:::format_comment(tibble:::pre_dots(tibble:::format_footer(x, squeezed)),
                        width = width)
footer <- tibble:::format_comment(tibble:::pre_dots(
  tibble:::format_footer(x, squeezed)
), width = width)

comment
pillar::style_subtle(comment)
pillar::style_subtle(footer)
c(pillar::style_subtle(comment), mcf, pillar::style_subtle(footer))

# format.tbl

# print.tbl

x <- c(pillar::style_subtle(comment), mcf, pillar::style_subtle(footer))

tibble:::cat_line
tibble:::cat_line(x)
