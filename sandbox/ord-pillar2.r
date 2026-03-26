
# REVIEW: {pillar}-based revision to formatting.

library(devtools)
load_all()
library(pillar)

# TODO: New approach: Format 3 tibbles separately.

# HELPERS

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

.ord_element_abbr <- c(
  active = " ",
  score = "x",
  structure = "s",
  pinv_weight = "w"
)

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

# generate row IDs for artificial (scaffolding) coordinates
ctl_new_rowid_pillar.tbl_ord_scaf <- function(
    controller, x, width, ..., .element = " "
) {
  out <- NextMethod()
  rowid <- as.character(seq_len(nrow(x)))
  # expect `.element` to have length 1L or `nrow(x)`
  rowid <- paste0(.element, rowid)
  width <- max(nchar(as.character(rowid)))
  new_pillar(
    list(
      title = out$title,
      type = out$type,
      data = pillar_component(
        new_pillar_shaft(
          list(row_ids = rowid),
          width = width,
          class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )
}

# arguments
( x <- ordinate(iris, princomp, cols = 1:4) )
width = NULL
n = NULL
max_extra_cols = NULL
max_footer_lines = NULL

# need to establish `width` as maximum determined for both.
width <- pillar:::get_width_print(width)
max_extra_cols <- pillar:::get_max_extra_cols(max_extra_cols)
max_footer_lines <- pillar:::get_max_footer_lines(max_footer_lines)

# prep
n_dims <- get_ord_dims(x)
n <- get_n_ord_setup(n, n_dims)

# artificial coordinates (heads only)
# get_factor(x, .matrix = "dims") |>
#   mapply(n = n, FUN = head) |>
#   lapply(as_tibble) |>
#   bind_rows() |>
#   print() -> x_l
get_factor(x, .matrix = "rows") |> 
  as_tibble() |> 
  head(n = n[1L]) |> 
  print() -> x_tl
get_factor(x, .matrix = "cols") |> 
  as_tibble() |> 
  head(n = n[2L]) |> 
  print() -> x_bl
x_l <- bind_rows(x_tl, x_bl)
# row attributes
annotation_factor(x, .matrix = "rows") |> 
  head(n = n[1L]) |> 
  print() -> x_tr
# column attributes
annotation_factor(x, .matrix = "cols") |> 
  head(n = n[2L]) |> 
  print() -> x_br

# restrict scaffolding to at most 3 coordinates
if (ncol(x_l) > 3L) {
  x_tl <- x_tl[, seq(3L)]
  x_bl <- x_bl[, seq(3L)]
  x_l <- x_l[, seq(3L)]
  sep_dots <- TRUE
} else {
  sep_dots <- FALSE
}
# new classes for new methods
# TODO: After additional methods are implemented, may not need `head()` above.
class(x_tl) <- unique(c("tbl_ord_scaf", class(x_tl)))
class(x_bl) <- unique(c("tbl_ord_scaf", class(x_bl)))
class(x_tr) <- unique(c("tbl_ord_aug", class(x_tr)))
class(x_br) <- unique(c("tbl_ord_aug", class(x_br)))
# allocate width for separator
# TODO: May need to change after separator is made from row indices.
div_width <- if (sep_dots) 6L else 2L
elt_width <- 1L
buf_width <- div_width + elt_width
# allocate at most 2/3 of width to scaffolding coordinates
# TODO: Write `tbl_format_setup()` method that uses `NextMethod()`.
x_tl_su <- tbl_format_setup(
  x_tl,
  width = floor(width * 2/3) - buf_width,
  .element = .ord_element_abbr[x_tr$.element]
)
x_bl_su <- tbl_format_setup(x_bl, width = floor(width * 2/3) - buf_width)
x_l_su <- tbl_format_setup(x_l, width = floor(width * 2/3) - buf_width)
# separate rows and columns
# x_tl_su$tbl_sum <- c(`Rows` = ...)
# for consistent spacing of rows and columns
x_tl_su$body <- x_l_su$body[c(1L, 2L, 2L + seq(n[1L]))]
x_bl_su$body <- x_l_su$body[c(1L, 2L, 2L + n[1L] + seq(n[2L]))]
n_miss <- pmax(n_dims - n, 0L)
x_tl_su$rows_missing <- 0L
x_tl_su$rows_total <- NA_integer_
x_bl_su$rows_missing <- 0L
x_bl_su$rows_total <- NA_integer_
# replace row IDs


# RESUME; REDO THIS PER ABOVE CHANGE

# allocate indexing and formatting to row IDs


# allocate indexing and formatting to row IDs
# FIXME: Always include `.element`.
x1elt <- c(x2$.element[seq(n[1L])], x3$.element[seq(n[2L])])
x1act <- if (".element" %in% c(names(x2), names(x3))) {
  x1elt == "active"
} else {
  rep(TRUE, sum(n))
}
x1brk <- sort(unique(c(
  1L,
  n[1L] + 1L,
  which(x1elt[-1L] != x1elt[-length(x1elt)]) + 1L,
  sum(n) + 1L
)))
.ord_elements
x1idx <- do.call(c, lapply(diff(x1brk), seq))
x1idx <- paste0(.ord_element_abbr[x1elt], x1idx)
rownames(x1s$x) <- x1idx
# allocate remaining width to annotations
# FIXME: Seems like some width was not used.
width2 <- width - x1s$width - div_width
x2s <- tbl_format_setup(x2, width = width2)
x3s <- tbl_format_setup(x3, width = width2)


# get maximum width 



# EXPERIMENT

# pillar:::tbl_format_setup.tbl()
# function (x, width, ..., setup, n, max_extra_cols, max_footer_lines, 
#           focus) 
# {
"!!!!DEBUG tbl_format_setup.tbl()"
if (is.null(setup)) {
  tbl_sum <- tbl_sum(x)
  return(new_tbl_format_setup(width, tbl_sum, rows_total = NA_integer_))
} else {
  tbl_sum <- setup$tbl_sum
}
rows <- tbl_nrow(x)
lazy <- is.na(rows)
if (lazy) {
  max <- attr(n, "max") %||% n
  df <- as.data.frame(head(x, max + 1))
  if (nrow(df) <= max) {
    rows <- nrow(df)
    n <- rows
  }
  else {
    df <- vec_head(df, n)
  }
} else {
  df <- df_head(x, n)
}
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
rownames(df) <- NULL
colonnade <- ctl_colonnade(df, has_row_id = if (!lazy && 
                                                .row_names_info(x) > 0) 
  "*"
  else TRUE, width = width, controller = x, focus = focus)
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
  max_footer_lines = max_footer_lines, abbrev_cols = abbrev_cols)
# }

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

# pillar:::ctl_new_rowid_pillar.tbl()
# function (controller, x, width, ..., title = NULL, type = NULL) 
# {
"!!!!DEBUG ctl_new_rowid_pillar.tbl(`v(width)`, `v(title)`)"
template <- names(ctl_new_pillar(controller, vector(), width, 
                                 title = title))
if (!length(template)) {
  return(NULL)
}
data <- rif_shaft(nrow(x))
out <- map(set_names(template), function(.x) "")
if ("type" %in% template) {
  out$type <- pillar_component(rif_type(identical(type, 
                                                  "*")))
}
if ("data" %in% template) {
  out$data <- pillar_component(data)
}
new_pillar(out, width = get_width(data))
# }

# from vignette
# https://pillar.r-lib.org/articles/extending.html
ctl_new_rowid_pillar.pillar_roman <- function(controller, x, width, ...) {
  out <- NextMethod()
  rowid <- utils::as.roman(seq_len(nrow(x)))
  width <- max(nchar(as.character(rowid)))
  new_pillar(
    list(
      title = out$title,
      type = out$type,
      data = pillar_component(
        new_pillar_shaft(list(row_ids = rowid),
                         width = width,
                         class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )
}

# TEST
rowide <- paste0(" ", as.character(seq_len(nrow(x_tl))))
widthe <- max(nchar(as.character(rowide)))
shafte <- new_pillar_shaft(
  list(row_ids = rowide),
  width = widthe,
  class = "pillar_rif_shaft"
)


