#' Formatting and printing methods for \code{tbl_ord}s
#' 
#' The \code{format} and \code{print} methods for class \code{"tbl_ord"} are
#' adapted from those for class \code{"tbl_df"} in the
#' \strong{\link[tibble]{tibble}} package and for class \code{"tbl_graph"} in
#' the \strong{\link[tidygraph]{tidygraph}} package.
#' 
#' Note: The \code{format} function is tedius but cannot be easily modularized 
#' without invoking \code{get_*()} and \code{augment_*()} multiple times, 
#' thereby significantly reducing performance.
#' 

#' @name formatting
#' @param x An ordination object.
#' @inheritParams tibble::format.tbl_df
#' @param ... Additional arguments.

#' @rdname formatting
#' @export
format.tbl_ord <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  
  # dimensional parameters
  uv <- get_factor(x, .matrix = "uv", align = TRUE)
  uv_dims <- sapply(uv, dim)
  x_coord <- get_coord(x, align = TRUE)
  rk <- length(x_coord)
  uv_ann <- rlang::set_names(mapply(
    bind_cols,
    augment_factor(x, .matrix = "uv"),
    factor_annot(x, .matrix = "uv"),
    SIMPLIFY = FALSE
  ), c("u", "v"))
  n_ann <- sapply(uv_ann, ncol)
  if (is.null(n)) {
    n <- ifelse(
      uv_dims[1, ] > tbl_ord_opt("print_max"),
      tbl_ord_opt("print_min"),
      uv_dims[1, ]
    )
  }
  width <- width %||% tbl_ord_opt("width") %||% getOption("width")
  uv_extra <- rlang::set_names(rep(
    n_extra %||% tbl_ord_opt("max_extra_cols"),
    length.out = 2
  ), c("u", "v"))
  
  # headers!
  prev_class <- setdiff(class(x), "tbl_ord")[1]
  tbl_ord_descr <- if (!is.null(prev_class) && prev_class != "list") {
    paste0("# A '", prev_class, "' tbl_ord")
  } else {
    paste0("# A tbl_ord")
  }
  header <- paste0(
    tbl_ord_descr,
    ": (", uv_dims[1, 1], " x ", rk, ") x (", uv_dims[1, 2], " x ", rk, ")'"
  )
  coord_sum <- paste0(
    "# ", rk,
    " coordinate", if(rk == 1) "" else "s",
    if (is.null(attr(x, "alignment"))) "" else ", realigned from originals",
    ": ",
    print_reps(x_coord)
  )
  
  # format U and V together first, then split
  uv_sums <- rlang::set_names(paste0(
    "# ", c("U", "V"),
    ": [ ", uv_dims[1, ], " x ", uv_dims[2, ], " | ", n_ann, " ]"
  ), c("u", "v"))
  fmts_coord <- format(dplyr::select(
    rbind(
      as_tibble(uv$u)[1:n[1], , drop = FALSE],
      as_tibble(uv$v)[1:n[2], , drop = FALSE]
    ),
    1:min(rk, 3)
  ), n = sum(n), width = width)
  wh_rows <- which(stringr::str_detect(fmts_coord, "^ *[0-9]+ "))
  id_width <- diff(as.vector(unique(stringr::str_locate(
    fmts_coord,
    "^ *[0-9]+ "
  )[wh_rows, , drop = FALSE])))
  fmt_coord <- list(
    u = unname(c(
      uv_sums["u"],
      fmts_coord[2],
      stringr::str_pad("", nchar(fmts_coord[2])),
      fmts_coord[wh_rows[1:n[1]]]
    )),
    v = unname(c(
      uv_sums["v"],
      fmts_coord[2],
      stringr::str_pad("", nchar(fmts_coord[2])),
      stringr::str_replace(
        fmts_coord[wh_rows[n[1] + 1:n[2]]],
        "^ *[0-9]+ ",
        paste0(format(1:n[2], width = id_width), " ")
      )
    ))
  )
  coord_width <- unique(nchar(fmts_coord)[-1])
  
  uv_footer <- rlang::set_names(c("", ""), c("u", "v"))
  fmt_ann <- lapply(c("u", "v"), function(.matrix) {
    fmt <- uv_ann[[.matrix]]
    if (ncol(fmt) == 0) return("")
    fmt <- format(fmt, n = n[.matrix], width = width - coord_width - 7)
    # blank header
    fmt[1] <- paste(rep(" ", times = nchar(fmt[2])), collapse = "")
    # remove footer
    if (grepl("^#", fmt[length(fmt)])) {
      uv_footer[.matrix] <<- fmt[length(fmt)]
      fmt <- fmt[-length(fmt)]
    }
    # remove row indices
    fmt <- substring(
      fmt,
      nchar(gsub("(^[0-9]+ ).*$", "\\1", fmt[length(fmt)])) + 1
    )
    fmt
  })
  uv_footer <- stringr::str_replace_all(
    uv_footer,
    "#",
    paste0("#", stringr::str_pad("", coord_width + ifelse(rk > 3, 4, 0)))
  )
  
  seps <- if (rk > 3) c("    ", " ...") else c("", "")
  fmt_seps <- mapply(
    function(x, y) {
      sep_dots_rows <- ceiling(c(2, (y - 2) / 2 + 2))
      c(paste(rep(" ", times = max(0, x)), collapse = ""),
        paste0(ifelse(2:y %in% sep_dots_rows, seps[2], seps[1]), " | "))
    },
    x = 3 + nchar(seps) + coord_width -
      sapply(fmt_coord, function(z) nchar(z[1])),
    y = sapply(fmt_coord, length),
    SIMPLIFY = FALSE
  )
  uv_fmt <- mapply(
    paste0,
    fmt_coord, fmt_seps, fmt_ann,
    SIMPLIFY = FALSE
  )
  uv_fmt <- mapply(
    function(fmt, footer) if (footer == "") fmt else c(fmt, footer),
    fmt = uv_fmt,
    footer = uv_footer,
    SIMPLIFY = FALSE
  )
  
  c(header, coord_sum, "# ", uv_fmt[[1]], "# ", uv_fmt[[2]])
}

#' @rdname formatting
#' @export
print.tbl_ord <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  fmt <- format(x, ..., n = n, width = width, n_extra = n_extra)
  cat(paste0(fmt, collapse = "\n"), sep = "")
  invisible(x)
}

`%||%` <- rlang::`%||%`

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
    "4" = paste0(paste(x[1:2], collapse = ", "), ", ..., ", x[length(x)])
  )
}