#' @title Biplots following the grammar of graphics
#' 
#' @description Build a biplot visualization from ordination data wrapped as a
#' [tbl_ord] object.
#' 

#' @details
#'
#' `ggbiplot()` produces a [ggplot][ggplot2::ggplot] object from a [tbl_ord]
#' object `ordination`. The baseline object is the default unadorned
#' `"ggplot"`-class object `p` with the following differences from what
#' [ggplot2::ggplot()] returns:
#' 
#' - `p$mapping` is augmented with `.matrix = .matrix`, which expects either
#' `.matrix = "rows"` or `.matrix = "cols"` from the biplot.

#' - `p$coordinates` is defaulted to [ggplot2::coord_equal()] in order to
#' faithfully render the geometry of an ordination. The optional parameters
#' `xlim`, `ylim`, `expand`, and `clip` are passed to `coord_equal()` and
#' default to its **ggplot2** defaults.

#' - When `x` or `y` are mapped to coordinates of `ordination`, and if
#' `axis.percents` is `TRUE`, `p$labels$x` or `p$labels$y` are defaulted to the
#' coordinate names concatenated with the percentages of [inertia][conference]
#' captured by the coordinates.

#' - `p` is assigned the class `"ggbiplot"` in addition to `"ggplot"`. This
#' serves no functional purpose currently.
#' 

#' Furthermore, the user may feed single integer values to the `x` and `y`
#' aesthetics, which will be interpreted as the corresponding coordinates in the
#' ordination. Currently only 2-dimensional biplots are supported, so both `x`
#' and `y` must take coordinate values.
#'
#' `ord_aes()` is a convenience function that generates a full-rank set of
#' coordinate aesthetics `..coord1`, `..coord2`, etc. mapped to the shared
#' coordinates of the ordination object, along with any additional aesthetics
#' that are processed internally by [ggplot2::aes()].
#'
#' Predictive axes are scaled inversely by the squared norm. By default, this is
#' computed from `x` and `y` (or from the full model space if obtained by
#' [ordr::ord_aes()]). If the data contain an `axis.ss` column, it is used
#' instead. Gower & Hand (1996) and Gower, Gardner--Lubbe, & le Roux (2011)
#' thoroughly explain the construction and interpretation of predictive biplots.
#' (Predictive calibration was previously handled by `ggbiplot(axis.type)` but
#' has been transferred to [`gggda::geom_axis(axis.type)`][gggda::geom_axis].)
#' 

#' @template biplot-layers

#' @template ref-gower1996
#' @template ref-gower2011

#' @name ggbiplot
#' @import ggplot2
#' @param ordination A [tbl_ord].
#' @param mapping List of default aesthetic mappings to use for the biplot. The
#'   default assigns the first two coordinates to the aesthetics `x` and `y`.
#'   Other assignments must be supplied in each layer added to the plot.
#' @inheritParams ggplot2::coord_cartesian
#' @param axis.percents Whether to concatenate default axis labels with inertia
#'   percentages.
#' @param sec.axes Matrix factor character to specify a secondary set of axes.
#' @param scale.factor Either a numeric value, used to scale the secondary axes
#'   against the primary axes, or the name of a harmonizing function (currently
#'   `"range"` or `"inertia"`); ignored if `sec.axes` is not specified.
#' @param scale_rows,scale_cols Either the character name of a numeric variable
#'   in `get_*(ordination)` or a numeric vector of length
#'   `nrow(get_*(ordination))`, used to scale the coordinates of the matrix
#'   factors.
#' @param ... Additional arguments passed to [ggplot2::fortify()]; see
#'   [fortify.tbl_ord()].
#' @return A [ggplot][ggplot2::ggplot] object.
#' @example inst/examples/ex-ggbiplot-secaxis-iris.r
#' @example inst/examples/ex-ggbiplot-lm-mtcars.r
#' @example inst/examples/ex-ggbiplot-prediction-iris.r
#' @seealso [ggplot2::ggplot2()], on which `ggbiplot()` is built

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
    ordination = NULL, mapping = aes(x = 1, y = 2),
    xlim = NULL, ylim = NULL, expand = TRUE, clip = "on",
    axis.percents = TRUE, sec.axes = NULL, scale.factor = "inertia",
    scale_rows = NULL, scale_cols = NULL,
    ...
) {
  dots <- match.call(expand.dots = FALSE)[["..."]]
  if ("axis.type" %in% names(dots)) {
    stop(
      "`axis.type` is no longer an argument to `ggbiplot()`; ",
      "specify it on an axis or rule layer, e.g. ",
      "`geom_cols_axis(axis.type = \"predictive\")`.",
      call. = FALSE
    )
  }

  if (axis.percents) {
    # store inertia
    inertia <- recover_inertia(ordination)
    if (all(is.na(inertia))) {
      axis.percents <- FALSE
    }
  }
  
  # fortify `ordination` if necessary
  if (! is.null(ordination)) ordination <- fortify(ordination, ...)
  
  # augment `mapping`, if necessary, with default coordinates
  mapping <- ensure_xy_aes(ordination, mapping)
  
  # augment `mapping`, if necessary, with `.element`
  if (! is.null(ordination)) {
    if (! ".element" %in% names(ordination))
      ordination$.element <- NA_character_
    mapping$.element <- {{ rlang::sym(".element") }}
  }
  
  # scale 'rows' or 'cols' as indicated by `scale_rows` and `scale_cols`
  if (! is.null(scale_rows) && ! is.null(ordination)) {
    ordination <- scale_ord(ordination, "rows", mapping, scale_rows)
  }
  if (! is.null(scale_cols) && ! is.null(ordination)) {
    ordination <- scale_ord(ordination, "cols", mapping, scale_cols)
  }
  
  # if `sec.axes` is specified, then fortify `ordination` and
  # scale the secondary axis coordinates to match the primary axis
  if (! is.null(sec.axes)) {
    
    sec.axes <- match_factor(sec.axes)
    if (! sec.axes %in% c("rows", "cols")) {
      stop("Select one matrix factor, 'rows' or 'cols', for secondary axes.")
    }
    pri.axes <- setdiff(c("rows", "cols"), sec.axes)
    
    # apply specified harmonization function
    if (is.character(scale.factor)) {
      scale.factor <- match.arg(scale.factor, c("range", "inertia"))
      scale.factor <- switch(
        scale.factor,
        range = harmonize_range(ordination, sec.axes),
        inertia = harmonize_inertia(ordination, sec.axes)
      )
    }
    
    # rescale any shared coordinates present
    if (! is.null(ordination)) ordination <- dplyr::mutate(
      ordination,
      dplyr::across(
        tidyselect::any_of(get_coord(ordination)),
        ~ ifelse(ordination$.matrix == sec.axes, . * scale.factor, .)
      )
    )
    
  }
  
  # conventional `ggplot()` call
  p <- ggplot(
    data = ordination,
    mapping = mapping,
    environment = parent.frame()
  )
  # `.matrix` aesthetic indicating whether to plot cases or variables
  if (! is.null(ordination)) {
    p$mapping$.matrix = {{ rlang::sym(".matrix") }}
  }
  
  # if `sec.axes` is specified, then add secondary axes
  if (! is.null(sec.axes)) {
    # FIXME: This approach is vulnerable to downstream `x` and `y` scales.
    p <- p + scale_x_continuous(sec.axis = sec_axis(~ . / scale.factor))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~ . / scale.factor))
  }
  
  # synchronize the scales of the axes
  p$coordinates <- coord_equal(
    xlim = xlim, ylim = ylim, expand = expand, clip = clip
  )
  # this is the biplot default; prevent message when modified
  p$coordinates$default <- TRUE
  
  # assign default axis labels
  if (axis.percents) {
    xy <- match(sapply(mapping, all.vars), get_coord(ordination))
    xy_aes <- get_coord(ordination)[xy]
    inertia_pct <- scales::percent(inertia / sum(inertia))
    if (! is.na(xy[1])) {
      p$labels$x <- paste0(xy_aes[1], " (", inertia_pct[xy[1]], ")")
    }
    if (! is.na(xy[2])) {
      p$labels$y <- paste0(xy_aes[2], " (", inertia_pct[xy[2]], ")")
    }
  }
  
  # add class label for potential future use
  class(p) <- c("ggbiplot", class(p))
  
  p
}

# interpret numerical x and y coordinates as coordinates;
# assume first two coordinates if none are provided
ensure_xy_aes <- function(ordination, mapping) {
  if (is.null(ordination)) return(aes())
  coords <- get_coord(ordination)
  coord_vars <- syms(coords)
  
  if (is.null(mapping$y)) {
    if (length(coords) < 2L) {
      stop("Ordination has too few coordinates; check `get_coord(<tbl_ord>)`.")
    }
    mapping$y <- {{ coord_vars[[2L]] }}
  } else {
    if (is.numeric(mapping$y) && length(mapping$y) == 1) {
      mapping$y <- {{ coord_vars[[mapping$y]] }}
    }
  }
  
  if (is.null(mapping$x)) {
    mapping$x <- {{ coord_vars[[1L]] }}
  } else {
    if (is.numeric(mapping$x) && length(mapping$x) == 1) {
      mapping$x <- {{ coord_vars[[mapping$x]] }}
    }
  }
  
  mapping
}

# use `.m` to avoid conflict with '.matrix' column in `ordination`
scale_ord <- function(ordination, .m, mapping, scale) {
  if (is.character(scale)) scale <- ordination[[scale]]
  ord_vars <- stringr::str_remove(as.character(mapping[c("x", "y")]), "^~")
  dplyr::mutate(
    ordination,
    dplyr::across(
      tidyselect::all_of(ord_vars),
      ~ ifelse(ordination$.matrix == .m, . * scale, .)
    )
  )
}

# functions to harmonize row and column axes
harmonize_range <- function(ordination, sec.axes) {
  pri.axes <- setdiff(c("rows", "cols"), sec.axes)
  ps_lim <- lapply(c(pri.axes, sec.axes), function(.m) {
    apply(
      # recover coordinates stored as attribute during `fortify()`
      ordination[ordination$.matrix == .m, get_coord(ordination)[seq(2L)]],
      2L, range
    )
  })
  ps_sf <- ps_lim[[1L]] / ps_lim[[2L]]
  min(ps_sf[ps_sf > 0])
}
harmonize_inertia <- function(ordination, sec.axes) {
  pri.axes <- setdiff(c("rows", "cols"), sec.axes)
  u_mat <- as.matrix(ordination[ordination$.matrix == pri.axes,
                                get_coord(ordination)[seq(2L)]])
  v_mat <- as.matrix(ordination[ordination$.matrix == sec.axes,
                                get_coord(ordination)[seq(2L)]])
  u_nobs <- nrow(u_mat); v_nobs <- nrow(v_mat)
  lambda_sq <-
    (v_nobs * sum(diag(u_mat %*% t(u_mat)))) /
    (u_nobs * sum(diag(v_mat %*% t(v_mat))))
  sqrt(lambda_sq)
}

#' @rdname ggbiplot
#' @export
ord_aes <- function(ordination, ...) {
  coords <- get_coord(ordination)
  mapping <- aes(...)
  for (i in seq_along(coords)) {
    mapping[[paste0("..coord", i)]] <- {{ rlang::sym(coords[[i]]) }}
  }
  mapping
}
