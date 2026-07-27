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
#' **Note:** The `format()` function is tedious but cannot be easily modularized
#' without invoking [recoverers], [annotation], and [augmentation] multiple
#' times, thereby significantly reducing performance.
#' 

#' @name format
#' @importFrom rlang "%||%"
#' @importFrom utils head
#' @importFrom crayon has_color
#' @importFrom pillar style_subtle
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

.by <- "\u00d7"
.by.sep <- " \u00d7 "
.times <- "\u00b7"
.times.sep <- " \u00b7 "
.prime <- "\u00b4"

# helper functions

strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*[a-zA-Z]", "", x)
}

style_type <- function(x) {
  if (crayon::has_color()) cli::style_italic(style_subtle(x)) else x
}

resolve_n <- function(n, n_dims) {
  if (is.null(n)) n <- list(NULL)
  if (length(n) == 1L) n <- rep(n, 2L)
  n <- n[seq(2L)]
  n <- vapply(seq_along(n), function(i) {
    if (is.null(n[[i]])) {
      if (n_dims[i] > tbl_ord_opt("print_max")) {
        tbl_ord_opt("print_min")
      } else {
        n_dims[i]
      }
    } else if (is.infinite(n[[i]])) {
      n_dims[i]
    } else {
      as.integer(n[[i]])
    }
  }, integer(1))
  n
}

get_ord_layout <- function(x, width = NULL, n = NULL,
                           max_extra_cols = NULL, max_footer_lines = NULL) {
  width <- width %||% tbl_ord_opt("width") %||% getOption("width")
  dims <- get_factor(x, .matrix = "dims")
  n_dims <- vapply(dims, nrow, 0L)
  coord <- get_coord(x)
  rk <- length(coord)
  dims_ann <- mapply(
    bind_cols,
    annotation_factor(x, .matrix = "dims"),
    SIMPLIFY = FALSE
  )
  names(dims_ann) <- c("rows", "cols")
  n_ann <- vapply(dims_ann, ncol, 0L)
  n <- resolve_n(n, n_dims)
  max_extra_cols <- max_extra_cols %||% tbl_ord_opt("max_extra_cols")
  max_footer_lines <- max_footer_lines %||% 7L
  conference <- get_conference(x)
  supp_rows <- attr(x, "rows_supplement")
  supp_cols <- attr(x, "cols_supplement")
  n_supp <- c(
    rows = if (! is.null(supp_rows)) nrow(supp_rows) else 0L,
    cols = if (! is.null(supp_cols)) nrow(supp_cols) else 0L
  )
  prev_class <- setdiff(class(x), "tbl_ord")[1]
  if (! is.null(prev_class) && prev_class == "list") prev_class <- NULL
  list(
    x = x, width = width, dims = dims, n_dims = n_dims,
    coord = coord, rk = rk,
    dims_ann = dims_ann, n_ann = n_ann,
    n = n, n_show = n,
    max_extra_cols = max_extra_cols,
    max_footer_lines = max_footer_lines,
    conference = conference,
    n_supp = n_supp, prev_class = prev_class
  )
}

ord_header_coords <- function(layout) {
  if (layout$rk == 0L) return(NULL)
  nm <- paste0("# ", layout$rk, " coordinate", if (layout$rk > 1) "s", ": ")
  names_str <- print_reps(layout$coord)
  full <- paste0(nm, names_str)
  if (nchar(full) <= layout$width) return(full)
  abbrev <- paste0("# ", layout$rk, " coordinate", if (layout$rk > 1) "s")
  if (nchar(abbrev) <= layout$width) return(abbrev)
  NULL
}

ord_header_supp <- function(layout) {
  rows_n <- layout$n_supp[["rows"]]
  cols_n <- layout$n_supp[["cols"]]
  if (rows_n == 0 && cols_n == 0) return(NULL)
  parts <- character()
  if (rows_n > 0) {
    parts <- c(parts, paste0(rows_n, " more row factor",
                             if (rows_n != 1) "s"))
  }
  if (rows_n > 0 && cols_n > 0) parts <- c(parts, "and")
  if (cols_n > 0) {
    parts <- c(parts, paste0(cols_n, " more column factor",
                             if (cols_n != 1) "s"))
  }
  line <- paste0("# \u2139 ", paste(parts, collapse = " "))
  line_width <- nchar(gsub("\033\\[[0-9;]*[a-zA-Z]", "", line))
  if (line_width <= layout$width) line else NULL
}

ord_header_factors <- function(layout) {
  conf_name <- function(p) {
    if (is.null(p)) return(NULL)
    if (p == 0) return("standard")
    if (p == 1) return("principal")
    if (p == 0.5) return("symmetric")
    paste0(round(100 * p, 0), "% inertia")
  }
  conf_pct <- function(p) {
    if (is.null(p)) return(NULL)
    paste0(round(100 * p, 0), "%")
  }
  factor_names <- c("Rows", "Columns")
  lines <- character()
  for (i in seq_along(factor_names)) {
    cname <- conf_name(layout$conference[i])
    cpct <- conf_pct(layout$conference[i])
    # tier 1: name + percentage in parentheses
    label_np <- if (! is.null(cname) && ! is.null(cpct)) {
      paste0(" (", cname, ", ", cpct, ")")
    }
    line_np <- paste0(
      "# ", factor_names[i], label_np, ": [ ",
      layout$n_dims[i], " \u00d7 ", layout$rk,
      " | ", layout$n_ann[i], " ]"
    )
    if (! is.null(label_np) && nchar(line_np) <= layout$width) {
      lines <- c(lines, line_np)
      next
    }
    # tier 2: name only in parentheses
    label_n <- if (! is.null(cname)) paste0(" (", cname, ")")
    line_n <- paste0(
      "# ", factor_names[i], label_n, ": [ ",
      layout$n_dims[i], " \u00d7 ", layout$rk,
      " | ", layout$n_ann[i], " ]"
    )
    if (! is.null(label_n) && nchar(line_n) <= layout$width) {
      lines <- c(lines, line_n)
      next
    }
    # tier 3: percentage only in parentheses
    label_p <- if (! is.null(cpct)) paste0(" (", cpct, ")")
    line_p <- paste0(
      "# ", factor_names[i], label_p, ": [ ",
      layout$n_dims[i], " \u00d7 ", layout$rk,
      " | ", layout$n_ann[i], " ]"
    )
    if (! is.null(label_p) && nchar(line_p) <= layout$width) {
      lines <- c(lines, line_p)
      next
    }
    # tier 4: full brackets, no conference
    line_noconf <- paste0(
      "# ", factor_names[i], ": [ ",
      layout$n_dims[i], " \u00d7 ", layout$rk,
      " | ", layout$n_ann[i], " ]"
    )
    if (nchar(line_noconf) <= layout$width) {
      lines <- c(lines, line_noconf)
      next
    }
    # tier 5: compact, no brackets
    compact <- paste0("# ", factor_names[i], ": ",
                      layout$n_dims[i], " \u00d7 ", layout$rk)
    if (nchar(compact) <= layout$width) lines <- c(lines, compact)
  }
  lines
}

ord_header <- function(layout) {
  lines <- character()
  # tier 1: full with class in quotes
  header1 <- paste0(
    "# A tbl_ord",
    if (! is.null(layout$prev_class)) {
      paste0(" of class '", layout$prev_class, "'")
    },
    ": (", layout$n_dims[1], .by.sep, layout$rk, ")", .times.sep,
    "(", layout$n_dims[2], .by.sep, layout$rk, ")", .prime
  )
  if (nchar(header1) <= layout$width) {
    lines <- c(lines, header1)
  } else {
    # tier 2: abbreviated class in angle brackets
    abbr_class <- if (! is.null(layout$prev_class)) {
      substr(layout$prev_class, 1, max(3, nchar(layout$prev_class) %/% 2))
    }
    header2 <- paste0(
      "# A tbl_ord",
      if (! is.null(abbr_class)) {
        paste0(" <", abbr_class, ">")
      },
      ": (", layout$n_dims[1], .by.sep, layout$rk, ")", .times.sep,
      "(", layout$n_dims[2], .by.sep, layout$rk, ")", .prime
    )
    if (nchar(header2) <= layout$width) {
      lines <- c(lines, header2)
    } else {
      # tier 3: reduced spacing
      header3 <- paste0(
        "# A tbl_ord",
        if (! is.null(abbr_class)) {
          paste0(" <", abbr_class, ">")
        },
        ": (", layout$n_dims[1], .by, layout$rk, ")",
        .times,
        "(", layout$n_dims[2], .by, layout$rk, ")", .prime
      )
      if (nchar(header3) <= layout$width) {
        lines <- c(lines, header3)
      } else {
        # tier 4: no class, minimal spacing
        minimal <- paste0(
          "# A tbl_ord: (",
          layout$n_dims[1], .by, layout$rk, ")",
          .times,
          "(", layout$n_dims[2], .by, layout$rk, ")", .prime
        )
        lines <- c(lines, minimal)
      }
    }
  }
  coord_line <- ord_header_coords(layout)
  if (! is.null(coord_line)) lines <- c(lines, coord_line)
  supp_line <- ord_header_supp(layout)
  if (! is.null(supp_line)) lines <- c(lines, supp_line)
  factor_lines <- ord_header_factors(layout)
  lines <- c(lines, factor_lines)
  lines
}

ord_footer <- function(layout) {
  lines <- character()
  rows_shown <- layout$n_show
  idx <- 1L
  for (nm in c("rows", "cols")) {
    more <- layout$n_dims[[nm]] - rows_shown[[idx]]
    if (more > 0L) {
      label <- if (nm == "rows") "row" else "column"
      line <- paste0("# \u2139 ", big_mark(more), " more ", label,
                     if (more != 1L) "s")
      lines <- c(lines, line)
    }
    idx <- idx + 1L
  }
  if (is.finite(layout$max_extra_cols)) {
    for (nm in c("rows", "cols")) {
      ann <- layout$dims_ann[[nm]]
      n_total <- ncol(ann)
      n_real <- if (n_total == 1L && names(ann) == ".rows") 0L else n_total
      if (n_real > layout$max_extra_cols) {
        n_extra <- n_real - layout$max_extra_cols
        # list names and types of un-printed variables
        hidden_idx <- seq(layout$max_extra_cols + 1L, n_total)
        hidden_names <- names(ann)[hidden_idx]
        hidden_types <- vapply(ann[hidden_idx], function(x) {
          paste0("<", class(x)[1L], ">")
        }, character(1))
        var_str <- paste(
          mapply(
            function(nm, tp) paste0(nm, " ", tp),
            hidden_names,
            hidden_types
          ),
          collapse = ", "
        )
        # wrap at ~60 chars for readability
        if (nchar(var_str) > 60L) {
          var_str <- paste0(
            substr(var_str, 1L, 57L), "..."
          )
        }
        line <- paste0(
          "# \u2139 ", big_mark(n_extra), " more variable",
          if (n_extra != 1L) "s", ":\n#   ", var_str
        )
        lines <- c(lines, line)
      }
    }
  }
  has_more_rows <- any(
    (layout$n_dims - rows_shown) > 0L
  )
  if (has_more_rows) {
    lines <- c(lines, "# \u2139 Use `print(n = ...)` to see more elements")
  }
  if (length(lines) > layout$max_footer_lines) {
    lines <- lines[seq_len(layout$max_footer_lines)]
  }
  lines <- style_subtle(lines)
  lines
}

ord_width_alloc <- function(layout) {
  has_ann <- any(layout$n_ann > 0)
  sep_width <- if (has_ann) {
    if (layout$rk > 3L) 6L else 2L
  } else {
    0L
  }
  if (has_ann) {
    coord_avail <- floor((layout$width - sep_width - 1L) * 2 / 3)
  } else {
    coord_avail <- layout$width - 1L
  }
  max_coord_cols <- max(1L, min(layout$rk, floor(coord_avail / 8)))
  coord_avail <- min(coord_avail, max_coord_cols * 12L)
  ann_avail <- if (has_ann) {
    max(0L, layout$width - coord_avail - sep_width - 1L)
  } else {
    0L
  }
  list(
    coord_avail = coord_avail,
    ann_avail = ann_avail,
    sep_width = sep_width,
    has_ann = has_ann,
    max_coord_cols = max_coord_cols
  )
}

ord_n_show <- function(layout, coord_alloc) {
  layout$n_show <- layout$n
  layout
}

ord_format_ann <- function(layout, coord_alloc) {
  result <- list()
  for (nm in c("rows", "cols")) {
    ann <- layout$dims_ann[[nm]]
    n_total <- ncol(ann)
    if (n_total == 0L) {
      result[[nm]] <- list(lines = character(), n_cols = 0L, n_total = 0L)
      next
    }
    n_show_ann <- min(n_total, layout$max_extra_cols)
    ann_sub <- ann[, seq_len(n_show_ann), drop = FALSE]
    n_rows <- layout$n_show[[if (nm == "rows") 1L else 2L]]
    ann_sub <- head(ann_sub, n_rows)
    fmt <- strip_ansi(format(ann_sub, width = coord_alloc$ann_avail))
    # keep col names (line 2) and types (line 3) as header
    # strip title, row numbers, truncation info
    # at very narrow widths, tibble wraps title/info lines w/ no room for data
    if (length(fmt) > 3L && ! grepl("^#", fmt[2L])) {
      header <- sub("^\\s+", "", fmt[2:3])
      data_lines <- fmt[seq(4L, length(fmt))]
      # remove tibble truncation/info lines
      data_lines <- data_lines[! grepl("^#", data_lines)]
      # remove leading row numbers (digits + whitespace)
      data_lines <- sub("^\\s*[0-9]+\\s+", "", data_lines)
      # style types
      header[2L] <- style_type(header[2L])
      lines <- c(header, data_lines)
    } else {
      lines <- character()
    }
    result[[nm]] <- list(
      lines = lines,
      n_cols = n_show_ann,
      n_total = n_total
    )
  }
  result
}

ord_format_coord <- function(layout, coord_alloc) {
  rk <- layout$rk
  cols_to_show <- seq_len(min(rk, coord_alloc$max_coord_cols))
  coord_rows <- as_tibble(layout$dims$rows)[, cols_to_show, drop = FALSE]
  coord_cols <- as_tibble(layout$dims$cols)[, cols_to_show, drop = FALSE]
  # Ffrmat each factor separately to get independent row numbers
  fmt_rows <- strip_ansi(
    format(head(coord_rows, layout$n_show[1L]), width = coord_alloc$coord_avail)
  )
  fmt_cols <- strip_ansi(
    format(head(coord_cols, layout$n_show[2L]), width = coord_alloc$coord_avail)
  )
  # process rows: check if format has usable column headers
  if (! grepl("^#", fmt_rows[2L])) {
    header_rows <- fmt_rows[2:3]
    rows_data <- fmt_rows[-(1:3)]
    rows_data <- sub("^\\s*[0-9]+", "", rows_data)
    rows_data <- paste0(seq_along(rows_data), rows_data)
  } else {
    # at very narrow widths, tibble wraps title/info lines
    col_names <- names(coord_rows)
    types <- vapply(
      coord_rows,
      function(x) paste0("<", class(x)[1L], ">"),
      character(1)
    )
    header_rows <- c(
      paste0(" ", paste(col_names, collapse = " ")),
      paste0(" ", paste(types, collapse = " "))
    )
    rows_data <- character()
  }
  # process cols: check if format has usable column headers
  if (! grepl("^#", fmt_cols[2L])) {
    header_cols <- fmt_cols[2:3]
    cols_data <- fmt_cols[-(1:3)]
    cols_data <- sub("^\\s*[0-9]+", "", cols_data)
    cols_data <- paste0(seq_along(cols_data), cols_data)
  } else {
    # at very narrow widths, tibble wraps title/info lines
    col_names <- names(coord_cols)
    types <- vapply(
      coord_cols,
      function(x) paste0("<", class(x)[1L], ">"),
      character(1)
    )
    header_cols <- c(
      paste0(" ", paste(col_names, collapse = " ")),
      paste0(" ", paste(types, collapse = " "))
    )
    cols_data <- character()
  }
  # use the wider header (names + types) to align the | separator
  if (nchar(trimws(header_cols[1L], "right")) >=
      nchar(trimws(header_rows[1L], "right"))) {
    header <- header_cols
  } else {
    header <- header_rows
  }
  # pad all coord lines to the same width for aligned | separator
  coord_lines <- trimws(c(header, rows_data, cols_data), "right")
  max_width <- max(nchar(coord_lines))
  coord_lines <- paste0(coord_lines, vapply(
    max_width - nchar(coord_lines),
    function(n) paste(rep(" ", n), collapse = ""),
    character(1)
  ))
  # style types line
  coord_lines[2L] <- style_type(coord_lines[2L])
  # style row numbers in data lines (lines 3+) as grey
  if (length(coord_lines) > 2L) {
    data_idx <- seq(3L, length(coord_lines))
    for (i in data_idx) {
      m <- regexpr("^[0-9]+", coord_lines[i])
      if (m != -1L) {
        num_str <- regmatches(coord_lines[i], m)
        rest <- substr(
          coord_lines[i],
          attr(m, "match.length") + 1L,
          nchar(coord_lines[i])
        )
        coord_lines[i] <- paste0(style_subtle(num_str), rest)
      }
    }
  }
  list(
    lines = coord_lines,
    n_cols_shown = length(cols_to_show),
    has_more_cols = rk > coord_alloc$max_coord_cols,
    n_rows = layout$n_show[1L],
    n_cols = layout$n_show[2L]
  )
}

ord_split_coord <- function(fmt_coord, layout) {
  lines <- fmt_coord$lines
  # lines structure: names (1), types (2), row data (3+), col data
  header <- lines[1:2]
  data_start <- 3L
  n_row_data <- layout$n_show[1L]
  rows_data <- lines[seq(data_start, length.out = n_row_data)]
  cols_data <- lines[seq(
    data_start + n_row_data,
    length.out = length(lines) - data_start - n_row_data + 1L
  )]
  list(
    rows = c(header, rows_data),
    cols = c(header, cols_data),
    has_more_cols = fmt_coord$has_more_cols
  )
}

ord_combine <- function(split, fmt_ann, layout, coord_alloc) {
  result <- list()
  styled_sep <- style_subtle(" | ")
  for (nm in c("rows", "cols")) {
    coord_lines <- split[[nm]]
    ann <- fmt_ann[[nm]]
    max_len <- max(length(coord_lines), length(ann$lines))
    if (length(coord_lines) < max_len) {
      coord_lines <- c(coord_lines, rep("", max_len - length(coord_lines)))
    }
    if (length(ann$lines) < max_len) {
      ann_lines <- c(ann$lines, rep("", max_len - length(ann$lines)))
    } else {
      ann_lines <- ann$lines
    }
    sep <- rep(styled_sep, max_len)
    if (split$has_more_cols) {
      styled_ellipsis <- style_subtle(" \u2026")
      sep[1L] <- paste0(styled_ellipsis, styled_sep)
      sep[-1L] <- paste0("  ", sep[-1L])
      # add ellipsis to median data line at the same position
      n_data <- length(coord_lines) - 2L
      if (n_data > 0L) {
        median_idx <- 2L + floor((n_data + 1L) / 2L)
        sep[median_idx] <- paste0(styled_ellipsis, styled_sep)
      }
    }
    combined <- paste0(coord_lines, sep, ann_lines)
    result[[nm]] <- combined
  }
  result
}

ord_assemble <- function(header, body, footer) {
  rows_lines <- body$rows
  cols_lines <- body$cols
  # split header at "# Columns" line to relocate it
  cols_idx <- grep("^# Columns", header)
  if (length(cols_idx) > 0L) {
    hdr_rows <- header[seq_len(cols_idx[1L] - 1L)]
    cols_header <- header[cols_idx[1L]]
    hdr_after <- if (cols_idx[1L] < length(header)) {
      header[seq(cols_idx[1L] + 1L, length(header))]
    } else {
      character()
    }
  } else {
    hdr_rows <- header
    cols_header <- NULL
    hdr_after <- character()
  }
  # style header lines after splitting (ord_header no longer applies styling)
  hdr_rows <- style_subtle(hdr_rows)
  if (! is.null(cols_header)) cols_header <- style_subtle(cols_header)
  if (length(hdr_after) > 0L) hdr_after <- style_subtle(hdr_after)
  c(
    hdr_rows,
    rows_lines,
    cols_header,
    cols_lines,
    hdr_after,
    footer
  )
}

# methods

#' @rdname format
#' @export
format.tbl_ord <- function(
  x, width = NULL, ..., n = NULL,
  max_extra_cols = NULL, max_footer_lines = NULL
) {
  layout <- get_ord_layout(
    x, width = width, n = n,
    max_extra_cols = max_extra_cols, max_footer_lines = max_footer_lines
  )
  header <- ord_header(layout)
  footer <- ord_footer(layout)
  alloc <- ord_width_alloc(layout)
  layout <- ord_n_show(layout, alloc)
  fmt_coord <- ord_format_coord(layout, alloc)
  fmt_ann <- ord_format_ann(layout, alloc)
  split <- ord_split_coord(fmt_coord, layout)
  body <- ord_combine(split, fmt_ann, layout, alloc)
  out <- ord_assemble(header, body, footer)
  out
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

# borrow trick from {tibble}
op.tbl_ord <- list(
  tbl_ord.print_max = 10L,
  tbl_ord.print_min = 5L,
  tbl_ord.width = NULL,
  tbl_ord.max_extra_cols = 50L
)

tbl_ord_opt <- function(x) {
  x_tbl_ord <- paste0("tbl_ord.", x)
  res <- getOption(x_tbl_ord)
  if (! is.null(res)) {
    return(res)
  }
  
  x_tibble <- paste0("tibble.", x)
  res <- getOption(x_tibble)
  if (! is.null(res)) {
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
