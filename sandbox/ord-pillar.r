
# REVIEW: {pillar}-based revision to formatting.

library(devtools)
load_all()
library(pillar)
( x <- ordinate(iris, prcomp, cols = 1:4) )
width = NULL
n = NULL
max_extra_cols = NULL
max_footer_lines = NULL

# 
get_ord_dims <- function(x) {
  vapply(get_factor(x, .matrix = "dims"), nrow, 0L)
}

# 
ord_dims_desc <- function(x) {
  n_dims <- get_ord_dims(x)
  rk <- length(get_coord(x))
  paste0("(", n_dims[1], " x ", rk, ") x (", n_dims[2], " x ", rk, ")'")
}

# 
get_n_ord_setup <- function(n, dims) {
  if (is.null(n)) n <- list(NULL)
  if (length(n) == 1L) n <- rep(n, 2L)
  n <- n[seq(2L)]
  n <- ifelse(
    vapply(n, is.null, FALSE),
    # REVIEW: Should these options be deprecated per {tibble} updates?
    ifelse(
      n_dims > tbl_ord_opt("print_max"),
      tbl_ord_opt("print_min"),
      n_dims
    ),
    n
  )
  n <- unlist(n)
}

# METHODS

# 
tbl_sum.tbl_ord <- function(x) {
  cl <- setdiff(class(x), "tbl_ord")
  rk <- length(get_coord(x))
  descr <- paste0(
    "A tbl_ord",
    if (! is.null(cl) && cl != "list") {
      paste0(
        " of class", if (length(cl) > 1L) "es",
        " '", paste(cl, collapse = "', '"), "'"
      )
    }
  )
  res <- ord_dims_desc(x)
  names(res) <- descr
  res
}

# 
tbl_nrow.tbl_ord <- function(x, ...) {
  sum(vapply(get_factor(x, .matrix = "dims"), nrow, 0L))
}



# TODO: Change this to be `.matrix`-specific.
# In order to make spacing consistent between row and column printings,
# need to establish `width` as maximum determined for both.
width <- pillar:::get_width_print(width)
n <- pillar:::get_n_print(n, tbl_nrow(x))
max_extra_cols <- pillar:::get_max_extra_cols(max_extra_cols)
max_footer_lines <- pillar:::get_max_footer_lines(max_footer_lines)
.matrix <- "rows"
tbl_format_setup.tbl_ord <- function(
    x, width, ...,
    setup, n, max_extra_cols, max_footer_lines, focus
) {
  "!!!!DEBUG tbl_format_setup.tbl()"
  if (is.null(setup)) {
    tbl_sum <- tbl_sum(x)
    return(new_tbl_format_setup(width, tbl_sum, rows_total = NA_integer_))
  } else {
    tbl_sum <- setup$tbl_sum
  }
  rows <- tbl_nrow(x)
  
  # WARNING: The `lazy` option is dropped; `rows` must be known.
  n_dims <- get_ord_dims(x)
  n <- get_n_ord_setup(n, n_dims)
  rows_missing <- pmax(n_dims - n, 0L)
  
  dims_ann <- mapply(
    bind_cols,
    annotation_factor(x, .matrix = "dims"),
    #recover_aug_factor(x, .matrix = "dims"),
    SIMPLIFY = FALSE
  )
  names(dims_ann) <- c("rows", "cols")
  n_ann <- sapply(dims_ann, ncol)
  
  df <- bind_rows(
    head(bind_cols(dims$rows[, seq(min(rk, 3))], dims_ann$rows), n = n[[1]]),
    head(bind_cols(dims$cols[, seq(min(rk, 3))], dims_ann$cols), n = n[[2]])
  )
  df_rows <- 
  df_rows <- bind_cols(df_rows, )
  
  df_rows_coord <- head(as_tibble(dims$rows)[, seq(min(rk, 3))], n = n[[1]])
  df_cols_coord <- head(as_tibble(dims$cols)[, seq(min(rk, 3))], n = n[[2]])
  rownames(df_rows_coord) <- rownames(df_cols_coord) <- NULL
  
  
  
  colonnade <- ctl_colonnade(
    df,
    has_row_id = if (!lazy && .row_names_info(x) > 0) "*" else TRUE,
    width = width,
    controller = x,
    focus = focus
  )
  body <- colonnade$body
  extra_cols <- colonnade$extra_cols
  extra_cols_total <- length(extra_cols)
  if (extra_cols_total > max_extra_cols) {
    length(extra_cols) <- max_extra_cols
  }
  abbrev_cols <- colonnade$abbrev_cols
  new_tbl_format_setup(
    x = x, df = df, width = width, tbl_sum = tbl_sum, 
    body = body, rows_missing = rows_missing, rows_total = rows, 
    extra_cols = extra_cols, extra_cols_total = extra_cols_total, 
    max_footer_lines = max_footer_lines, abbrev_cols = abbrev_cols
  )
}

# pillar:::ctl_colonnade()
# function (x, has_row_id = TRUE, width = NULL, controller = new_tbl(), 
#           focus = NULL) 
# {
"!!!!DEBUG ctl_colonnade()"
x <- rlang:::new_data_frame(x, names = rlang::names2(x))
width <- get_width_print(width)
stopifnot(all(focus %in% names(x)))
n <- nrow(x)
nc <- ncol(x)
if (n == 0 || nc == 0) {
  return(new_colonnade_body(list(), extra_cols = x, abbrev_cols = character(), 
                            abbrev_col_idxs = numeric()))
}
if (is_false(has_row_id)) {
  rowid <- NULL
} else {
  rowid <- ctl_new_rowid_pillar(controller, x, width, title = NULL, 
                                type = if (isTRUE(has_row_id)) 
                                  NULL
                                else has_row_id)
}
if (is.null(rowid)) {
  rowid_width <- 0L
} else {
  rowid_width <- get_width(rowid)
}
has_star <- identical(has_row_id, "*")
tier_widths <- get_tier_widths(width, nc, rowid_width + 1L)
formatted_tiers <- list()
extra_cols <- list(a = 1)[0]
abbrev_cols <- character()
abbrev_col_idxs <- numeric()
on_tier <- function(formatted) {
  formatted_tiers <<- c(formatted_tiers, list(formatted))
}
on_extra_cols <- function(my_extra_cols) {
  new_extra_cols <- pmap(my_extra_cols, function(x, title, 
                                                 cols) {
    out <- as.list(x)[cols]
    if (is.null(title)) {
      return(out)
    }
    if (length(out) > 1) {
      title_empty <- rep_along(title, "")
      new_names <- paste0(paste0(title_empty, "$", 
                                 collapse = ""), names(out))
      new_names[[1]] <- paste0(paste0(title, "$", collapse = ""), 
                               names(out)[[1]])
      names(out) <- new_names
    }
    else {
      names(out) <- prepare_title(c(title, names(out)))
    }
    out
  })
  stopifnot(length(extra_cols) == 0)
  extra_cols <<- unlist(new_extra_cols, recursive = FALSE)
}
on_abbrev_col <- function(title) {
  abbrev_cols <<- c(abbrev_cols, title)
}
on_get_n_abbrev_cols <- function() {
  length(abbrev_cols)
}
cb <- new_emit_tiers_callbacks(
  controller, rowid, rowid_width, 
  has_star, on_tier, on_extra_cols, on_abbrev_col, on_get_n_abbrev_cols
)
do_emit_tiers(x, tier_widths, length(focus), cb, focus)
new_colonnade_body(formatted_tiers, extra_cols, abbrev_cols, 
                   abbrev_col_idxs)
# }



NBSP <- getFromNamespace("NBSP", "pillar")
format_comment <- getFromNamespace("format_comment", "pillar")


tbl_format_header.tbl_ord <- function(x, setup, ...) {
  # named_header <- setup$tbl_sum
  named_header <- tbl_sum(x)
  header <- paste0(
    align(paste0(rlang::names2(named_header), ":"), space = NBSP),
    " ", named_header
  )
  # style_subtle(format_comment(header, width = setup$width))
  style_subtle(format_comment(header, width = width))
}





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




inertia_name <- function(p) {
  if (p == 0) return("standard")
  if (p == 1) return("principal")
  if (p == 0.5) return("symmetric")
  paste0(round(100 * p, digits = 0L), "% inertia")
}

tbl_sum.tbl_fct <- function(x, ...) {
  rk <- attr(x, "rank")
  descr <- paste("[", nrow(x), "x", rk, "|", ncol(x) - rk, "]")
  varnm <- inertia_name(attr(x, "p"))
  names(descr) <- paste0(match_factor(attr(x, ".matrix")), " (", varnm, ")")
  descr
}

rows_tbl <- bind_cols(dims$rows, dims_ann$rows)
attr(rows_tbl, ".matrix") <- "rows"
attr(rows_tbl, "rank") <- length(coord)
attr(rows_tbl, "p") <- x_inertia[1L]
class(rows_tbl) <- c("tbl_fct", class(rows_tbl))
cols_tbl <- bind_cols(dims$cols, dims_ann$cols)
attr(cols_tbl, ".matrix") <- "cols"
attr(cols_tbl, "rank") <- length(coord)
attr(cols_tbl, "p") <- x_inertia[2L]
class(cols_tbl) <- c("tbl_fct", class(cols_tbl))

rows_tbl
cols_tbl
