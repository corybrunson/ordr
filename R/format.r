#' @title Format a tbl_ord for printing
#'
#' @description These methods of [base::format()] and [base::print()] render a
#'   (usually more) tidy readout of a [tbl_ord] object that is consistent across
#'   all original ordination classes.
#'
#' @details
#'
#' The `format` and `print` methods for class [tbl_ord] are adapted from
#' those for class [tbl_df][tibble::tbl_df] and for class
#' [tbl_graph][tidygraph::tbl_graph].
#'
#' **Note:** The `format()` function is tedius but cannot be easily modularized
#' without invoking [accessors], [annotation], and [augmentation] multiple
#' times, thereby significantly reducing performance.
#' 

#' @name format
#' @importFrom rlang "%||%"
#' @param x A [tbl_ord] object.
#' @inheritParams tibble::format.tbl
#' @param ... Additional arguments.
#' @example inst/examples/mtcars-lm-isolines.r
#' @example inst/examples/country-cmds-lm.r
#' @example inst/examples/finches-ca.r
#' @example inst/examples/iris-princomp-secondary.r
#' @example inst/examples/diabetes-lda-supplement.r

#' @rdname format
#' @export
format.tbl_ord <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  
  # raw components and parameters
  uv <- get_factor(x, .matrix = "uv", align = TRUE)
  n_uv <- sapply(uv, nrow)
  coord <- get_coord(x, align = TRUE)
  rk <- length(coord)
  uv_ann <- mapply(
    bind_cols,
    annotation_factor(x, .matrix = "uv"),
    #augmentation_factor(x, .matrix = "uv"),
    SIMPLIFY = FALSE
  )
  names(uv_ann) <- c("u", "v")
  n_ann <- sapply(uv_ann, ncol)
  if (is.null(n)) {
    n <- ifelse(
      n_uv > tbl_ord_opt("print_max"),
      tbl_ord_opt("print_min"),
      n_uv
    )
  }
  width <- width %||% tbl_ord_opt("width") %||% getOption("width")
  #uv_extra <- rep(
  #  n_extra %||% tbl_ord_opt("max_extra_cols"),
  #  length.out = 2
  #)
  #names(uv_extra) <- c("u", "v")
  
  # headers!
  prev_class <- setdiff(class(x), "tbl_ord")[1]
  tbl_ord_header <- paste0(
    "# A tbl_ord",
    if (!is.null(prev_class) && prev_class != "list") {
      paste0(" of class '", prev_class, "'")
    },
    ": (", n_uv[1], " x ", rk, ") x (", n_uv[2], " x ", rk, ")'"
  )
  coord_header <- paste0(
    "# ", rk,
    " coordinate", if(rk == 1) "" else "s",
    if (is.null(attr(x, "align"))) "" else ", transformed",
    ": ",
    print_reps(coord)
  )
  supp_header <- if (! is.null(attr(x, "u_supplement")) |
                     ! is.null(attr(x, "u_supplement"))) {
    paste0(
      "# ",
      if (! is.null(attr(x, "u_supplement"))) {
        paste0(nrow(attr(x, "u_supplement")), " supplementary rows")
      },
      if (! is.null(attr(x, "u_supplement")) &
          ! is.null(attr(x, "v_supplement"))) {
        " and "
      },
      if (! is.null(attr(x, "v_supplement"))) {
        paste0(nrow(attr(x, "v_supplement")), " supplementary columns")
      }
    )
  } else NULL
  uv_headers <- paste0(
    "# ", c("U", "V"),
    ": [ ", n_uv, " x ", rk, " | ", n_ann, " ]"
  )
  names(uv_headers) <- c("u", "v")
  
  # format U and V separately
  # (should format together, then split, in order to sync coordinates)
  fmt_coord_u <- format(
    as_tibble(uv$u)[1:n[1], 1:min(rk, 3), drop = FALSE],
    n = n[1], width = width / 2
  )
  fmt_coord_v <- format(
    as_tibble(uv$v)[1:n[2], 1:min(rk, 3), drop = FALSE],
    n = n[2], width = width / 2
  )
  fmt_coord <- list(
    u = unname(c(
      uv_headers["u"],
      fmt_coord_u[2],
      stringr::str_pad("", nchar(fmt_coord_u[2])),
      fmt_coord_u[4:length(fmt_coord_u)]
    )),
    v = unname(c(
      uv_headers["v"],
      fmt_coord_v[2],
      stringr::str_pad("", nchar(fmt_coord_v[2])),
      fmt_coord_v[4:length(fmt_coord_v)]
    ))
  )
  
  # footers?
  uv_footers <- n_uv - n > 0
  fmt_ann <- lapply(1:2, function(i) {
    if (ncol(uv_ann[[i]]) == 0) return("")
    # dodge `format.pillar_shaft_decimal()` errors
    wid_try <- (width - 7) / 2
    fmt_try <- try(
      c("", format(uv_ann[[i]], n = n[i], width = wid_try)[-1]),
      silent = TRUE
    )
    while (class(fmt_try) == "try-error") {
      wid_try <- wid_try - 1
      fmt_try <- c("", format(uv_ann[[i]], n = n[i], width = wid_try)[-1])
    }
    fmt_try
  })
  names(fmt_ann) <- c("u", "v")
  
  # separate coordinates from annotations
  seps <- if (rk > 3) c("    ", " ...") else c("", "")
  fmt_seps <- mapply(
    function(x, y) {
      sep_dots_rows <- ceiling(c(2, (y - 2) / 2 + 2))
      c(paste(rep(" ", times = max(0, x)), collapse = ""),
        paste0(ifelse(2:y %in% sep_dots_rows, seps[2], seps[1]), " | "))
    },
    x = 3 + nchar(seps) -
      sapply(fmt_coord, function(z) nchar(z[1])),
    y = sapply(fmt_coord, length),
    SIMPLIFY = FALSE
  )
  
  # paste together, with attention to footers
  for (i in 1:2) {
    if (uv_footers[i]) {
      fmt_coord[[i]] <- c(fmt_coord[[i]], "")
      fmt_seps[[i]] <- c(fmt_seps[[i]], "")
    }
  }
  fmt_uv <- mapply(
    paste0,
    fmt_coord, fmt_seps, fmt_ann,
    SIMPLIFY = FALSE
  )
  
  c(
    tbl_ord_header,
    coord_header,
    supp_header,
    "# ", fmt_uv[[1]], "# ", fmt_uv[[2]]
  )
}

#' @rdname format
#' @export
print.tbl_ord <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  fmt <- format(x, ..., n = n, width = width, n_extra = n_extra)
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
    "4" = paste0(paste(x[1:2], collapse = ", "), ", ..., ", x[length(x)])
  )
}
