#' Lay out panels in a grid
#'
#' `facet_ord()` forms a matrix of panels defined by row and column
#' faceting variables. It is most useful when you have two discrete
#' variables, and all combinations of the variables exist in the data.
#' If you have only one variable with many levels, try [facet_wrap()].
#'
#' @param rows,cols A set of variables or expressions quoted by
#'   [vars()] and defining faceting groups on the rows or columns
#'   dimension. The variables can be named (the names are passed to
#'   `labeller`).
#'
#'   For compatibility with the classic interface, `rows` can also be
#'   a formula with the rows (of the tabular display) on the LHS and
#'   the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this
#'   dimension (either row or column).
#' @param scales Are scales shared across all facets (the default,
#'   `"fixed"`), or do they vary across rows (`"free_x"`),
#'   columns (`"free_y"`), or both rows and columns (`"free"`)?
#' @param space If `"fixed"`, the default, all panels have the same size.
#'   If `"free_y"` their height will be proportional to the length of the
#'   y scale; if `"free_x"` their width will be proportional to the
#'  length of the x scale; or if `"free"` both height and width will
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes one data frame of labels and returns a
#'   list or data frame of character vectors. Each input column corresponds to
#'   one factor. Thus there will be more than one with `vars(cyl, am)`. Each
#'   output column gets displayed as one separate line in the strip label. This
#'   function should inherit from the "labeller" S3 class for compatibility with
#'   [labeller()]. You can use different labeling functions for different kind
#'   of labels, for example use [label_parsed()] for formatting facet labels.
#'   [label_value()] is used by default, check it for more details and pointers
#'   to other options.
#' @param as.table If `TRUE`, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If `FALSE`, the
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param switch By default, the labels are displayed on the top and
#'   right of the plot. If `"x"`, the top labels will be
#'   displayed to the bottom. If `"y"`, the right-hand side
#'   labels will be displayed to the left. Can also be set to
#'   `"both"`.
#' @param shrink If `TRUE`, will shrink scales to fit output of
#'   statistics, not raw data. If `FALSE`, will be range of raw data
#'   before statistical summary.
#' @param drop If `TRUE`, the default, all factor levels not used in the
#'   data will automatically be dropped. If `FALSE`, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @param margins Either a logical value or a character
#'   vector. Margins are additional facets which contain all the data
#'   for each of the possible values of the faceting variables. If
#'   `FALSE`, no additional facets are included (the
#'   default). If `TRUE`, margins are included for all faceting
#'   variables. If specified as a character vector, it is the names of
#'   variables for which margins are to be created.
#' @param facets `r lifecycle::badge("deprecated")` Please use `rows`
#'   and `cols` instead.
#' @param axes Determines which axes will be drawn. When `"margins"`
#'   (default), axes will be drawn at the exterior margins. `"all_x"` and
#'   `"all_y"` will draw the respective axes at the interior panels too, whereas
#'   `"all"` will draw all axes at all panels.
#' @param axis.labels Determines whether to draw labels for interior axes when
#'   the `axes` argument is not `"margins"`. When `"all"` (default), all
#'   interior axes get labels. When `"margins"`, only the exterior axes get
#'   labels and the interior axes get none. When `"all_x"` or `"all_y"`, only
#'   draws the labels at the interior axes in the x- or y-direction
#'   respectively.
#' @export
facet_ord <- function(
    rows = NULL, cols = NULL, scales = "fixed",
    space = "fixed", shrink = TRUE,
    labeller = "label_value", as.table = TRUE,
    switch = NULL, drop = TRUE, margins = FALSE,
    axes = "margins", axis.labels = "all",
    facets = rlang:::deprecated()
) {
  
  scales <- rlang::arg_match0(scales %||% "fixed", c("fixed", "free"))
  free <- list(x = scales == "free", y = scales == "free")
  
  space <- rlang::arg_match0(space %||% "fixed", c("fixed", "free"))
  space_free <- list(x = space == "free", y = space == "free")
  
  draw_axes <- rlang::arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = any(draw_axes %in% c("all_x", "all")),
    y = any(draw_axes %in% c("all_y", "all"))
  )
  
  # Omitting labels is special-cased internally, so even when no internal axes
  # are to be drawn, register as labelled.
  axis_labels <- rlang::arg_match0(axis.labels,
                                   c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )
  
  if (!is.null(switch)) {
    rlang::arg_match0(switch, c("both", "x", "y"))
  }
  
  facets_list <- grid_as_facets_list(rows, cols)
  
  # Check for deprecated labellers
  labeller <- ggplot2:::check_labeller(labeller)
  
  ggproto(
    NULL, FacetOrd,
    shrink = shrink,
    params = list(
      rows = facets_list$rows, cols = facets_list$cols, margins = margins,
      free = free, space_free = space_free, labeller = labeller,
      as.table = as.table, switch = switch, drop = drop,
      draw_axes = draw_axes, axis_labels = axis_labels
    )
  )
}

# Returns a list of quosures objects.
# The list has exactly two elements, `rows` and `cols`.
grid_as_facets_list <- function(rows, cols) {
  save(rows, cols,
       file = here::here("ignore/facet-ord-grid-as-facets-list.rda"))
  # load(here::here("ignore/facet-ord-grid-as-facets-list.rda"))
  
  is_rows_vars <- is.null(rows) || rlang::is_quosures(rows)
  if (!is_rows_vars) {
    if (!is.null(cols)) {
      msg <- paste0(
        "{.arg rows} must be {.code NULL} ",
        "or a {.fn vars} list if {.arg cols} is a {.fn vars} list."
      )
      # Native pipe have higher precedence than +
      # so any type of gg object can be
      # expected here, not just ggplot
      if (inherits(rows, "gg")) {
        msg <- c(
          msg,
          "i" = "Did you use {.code %>%} or {.code |>} instead of {.code +}?"
        )
      }
      cli::cli_abort(msg)
    }
    # For backward-compatibility
    facets_list <- ggplot2:::as_facets_list(rows)
    if (length(facets_list) > 2L) {
      cli::cli_abort(
        "A grid facet specification can't have more than two dimensions.")
    }
    # Fill with empty quosures
    facets <- list(rows = quos(), cols = quos())
    facets[seq_along(facets_list)] <- facets_list
    # Do not compact the legacy specs
    return(facets)
  }
  
  ggplot2:::check_object(cols, rlang::is_quosures,
                         "a {.fn vars} specification", allow_null = TRUE)
  
  list(
    rows = ggplot2:::compact_facets(ggplot2:::as_facets_list(rows)),
    cols = ggplot2:::compact_facets(ggplot2:::as_facets_list(cols))
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetOrd <- ggproto(
  "FacetOrd", Facet,
  shrink = TRUE,
  
  setup_data = function(data, params) {
    save(data, params,
         file = here::here("ignore/facet-ord-setup-data.rda"))
    # load(here::here("ignore/facet-ord-setup-data.rda"))
    
    data
  },
  
  setup_params = function(data, params) {
    save(data, params,
         file = here::here("ignore/facet-ord-setup-params.rda"))
    # load(here::here("ignore/facet-ord-setup-params.rda"))
    
    params
  },
  
  compute_layout = function(self, data, params) {
    save(self, data, params,
         file = here::here("ignore/facet-ord-compute-layout.rda"))
    # load(here::here("ignore/facet-ord-compute-layout.rda"))
    
    rows <- params$rows
    cols <- params$cols
    
    ggplot2:::check_facet_vars(names(rows), names(cols),
                               name = snake_class(self))
    
    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      cli::cli_abort(c(
        "Faceting variables can only appear in {.arg rows} or {.arg cols}, ",
        "not both.",
        "i" = "Duplicated variables: {.val {dups}}"
      ), call = call2(snake_class(self)))
    }
    
    base_rows <- combine_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
      base_rows[] <- lapply(base_rows, rev_order)
    }
    base_cols <- combine_vars(data, params$plot_env, cols, drop = params$drop)
    base <- ggplot2:::df.grid(base_rows, base_cols)
    
    if (nrow(base) == 0) {
      return(ggplot2:::data_frame0(
        PANEL = factor(1L),
        ROW = 1L,
        COL = 1L,
        SCALE_X = 1L,
        SCALE_Y = 1L
      ))
    }
    
    # Add margins
    base <- ggplot2:::reshape_add_margins(
      base, list(names(rows), names(cols)), params$margins)
    base <- ggplot2:::unique0(base)
    
    # Create panel info dataset
    panel <- ggplot2:::id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))
    
    rows <- 
      if (!length(names(rows))) rep(1L, length(panel)) else
        ggplot2:::id(base[names(rows)], drop = TRUE)
    cols <- 
      if (!length(names(cols))) rep(1L, length(panel)) else 
        ggplot2:::id(base[names(cols)], drop = TRUE)
    
    panels <- ggplot2:::data_frame0(PANEL = panel, ROW = rows, COL = cols, base)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL
    
    panels$SCALE_X <- if (params$free$x) panels$COL else 1L
    panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L
    
    panels
  },
  
  map_data = function(data, layout, params) {
    save(data, layout, params,
         file = here::here("ignore/facet-ord-map-data.rda"))
    # load(here::here("ignore/facet-ord-map-data.rda"))
    
    if (ggplot2:::empty(data)) {
      return(vctrs::vec_cbind(data %|W|% NULL, PANEL = integer(0)))
    }
    
    rows <- params$rows
    cols <- params$cols
    vars <- c(names(rows), names(cols))
    
    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }
    
    # Compute faceting values and add margins
    margin_vars <- list(intersect(names(rows), names(data)),
                        intersect(names(cols), names(data)))
    data <- ggplot2:::reshape_add_margins(data, margin_vars, params$margins)
    
    facet_vals <- 
      ggplot2:::eval_facets(c(rows, cols), data, params$.possible_columns)
    
    # If any faceting variables are missing, add them in by
    # duplicating the data
    missing_facets <- setdiff(vars, names(facet_vals))
    if (length(missing_facets) > 0) {
      to_add <- ggplot2:::unique0(layout[missing_facets])
      
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      
      data <- ggplot2:::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- ggplot2:::unrowname(vctrs::vec_cbind(
        ggplot2:::unrowname(facet_vals[data_rep, ,  drop = FALSE]),
        ggplot2:::unrowname(to_add[facet_rep, , drop = FALSE]))
      )
    }
    
    # Add PANEL variable
    if (nrow(facet_vals) == 0) {
      # Special case of no faceting
      data$PANEL <- NO_PANEL
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      layout[] <- lapply(layout[], as.factor)
      
      keys <- ggplot2:::join_keys(facet_vals, layout, by = vars)
      
      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  
  draw_panels = function(
    panels, layout, x_scales, y_scales, ranges, coord, data, theme, params
  ) {
    save(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params,
         file = here::here("ignore/facet-ord-draw-panels.rda"))
    # load(here::here("ignore/facet-ord-draw-panels.rda"))
    
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      cli::cli_abort("{.fn {snake_class(coord)}} doesn't support free scales.")
    }
    
    if (!params$axis_labels$x) {
      cols <- seq_len(nrow(layout))
      x_axis_order <- as.integer(layout$PANEL[order(layout$ROW, layout$COL)])
    } else {
      cols <- which(layout$ROW == 1)
      x_axis_order <- layout$COL
    }
    if (!params$axis_labels$y) {
      rows <- seq_len(nrow(layout))
      y_axis_order <- as.integer(layout$PANEL[order(layout$ROW, layout$COL)])
    } else {
      rows <- which(layout$COL == 1)
      y_axis_order <- layout$ROW
    }
    
    ranges <- censor_labels(ranges, layout, params$axis_labels)
    axes <- 
      render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)
    
    col_vars <- ggplot2:::unique0(layout[names(params$cols)])
    row_vars <- ggplot2:::unique0(layout[names(params$rows)])
    # Adding labels metadata, useful for labellers
    attr(col_vars, "type") <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type") <- "rows"
    attr(row_vars, "facet") <- "grid"
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)
    
    aspect_ratio <- theme$aspect.ratio
    if (!is.null(aspect_ratio) && 
        (params$space_free$x || params$space_free$y)) {
      cli::cli_abort("Free scales cannot be mixed with a fixed aspect ratio.")
    }
    aspect_ratio <- aspect_ratio %||% coord$aspect(ranges[[1]])
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    mtx <- function(x) matrix(x, nrow = nrow, ncol = ncol, byrow = TRUE)
    panel_table <- mtx(panels)
    
    # @kohske
    # Now size of each panel is calculated using PANEL$ranges, which is given by
    # coord_train called by train_range.
    # So here, "scale" need not to be referred.
    #
    # In general, panel has all information for building facet.
    if (params$space_free$x) {
      ps <- layout$PANEL[layout$ROW == 1]
      widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
      panel_widths <- unit(widths, "null")
    } else {
      panel_widths <- rep(unit(1, "null"), ncol)
    }
    if (params$space_free$y) {
      ps <- layout$PANEL[layout$COL == 1]
      heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
      panel_heights <- unit(heights, "null")
    } else {
      panel_heights <- rep(unit(1 * abs(aspect_ratio), "null"), nrow)
    }
    
    panel_table <- gtable::gtable_matrix(
      "layout", panel_table,
      panel_widths, panel_heights, respect = respect,
      clip = coord$clip, z = mtx(1))
    panel_table$layout$name <- paste0(
      'panel-', rep(seq_len(nrow), ncol), '-', rep(seq_len(ncol), each = nrow))
    
    panel_table <- gtable::gtable_add_col_space(
      panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable::gtable_add_row_space(
      panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)
    
    # Add axes
    if (params$draw_axes$x) {
      axes$x <- lapply(axes$x, function(x) mtx(x[x_axis_order]))
      panel_table <- weave_axes(panel_table, axes$x)$panels
    } else {
      panel_table <- 
        gtable::gtable_add_rows(panel_table, max_height(axes$x$top), 0)
      panel_table <- 
        gtable::gtable_add_rows(panel_table, max_height(axes$x$bottom), -1)
      panel_pos_col <- panel_cols(panel_table)
      panel_table <- gtable::gtable_add_grob(
        panel_table, axes$x$top, 1, panel_pos_col$l, clip = "off",
        name = paste0("axis-t-", seq_along(axes$x$top)), z = 3)
      panel_table <- gtable::gtable_add_grob(
        panel_table, axes$x$bottom, -1, panel_pos_col$l, clip = "off",
        name = paste0("axis-b-", seq_along(axes$x$bottom)), z = 3)
    }
    
    if (params$draw_axes$y) {
      axes$y <- lapply(axes$y, function(y) mtx(y[y_axis_order]))
      panel_table <- weave_axes(panel_table, axes$y)$panels
    } else {
      panel_table <- gtable::gtable_add_cols(
        panel_table, max_width(axes$y$left), 0)
      panel_table <- gtable::gtable_add_cols(
        panel_table, max_width(axes$y$right), -1)
      panel_pos_rows <- panel_rows(panel_table)
      panel_table <- gtable::gtable_add_grob(
        panel_table, axes$y$left, panel_pos_rows$t, 1, clip = "off",
        name = paste0("axis-l-", seq_along(axes$y$left)), z = 3)
      panel_table <- gtable::gtable_add_grob(
        panel_table, axes$y$right, panel_pos_rows$t, -1, clip = "off",
        name = paste0("axis-r-", seq_along(axes$y$right)), z= 3)
    }
    
    # Add strips
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    inside_x <- 
      (theme$strip.placement.x %||% theme$strip.placement %||% "inside") ==
      "inside"
    inside_y <- 
      (theme$strip.placement.y %||% theme$strip.placement %||% "inside") ==
      "inside"
    strip_padding <- grid::convertUnit(theme$strip.switch.pad.grid, "cm")
    panel_pos_col <- panel_cols(panel_table)
    if (switch_x) {
      if (!is.null(strips$x$bottom)) {
        if (inside_x || all(vapply(axes$x$bottom, is.zero, logical(1)))) {
          panel_table <- gtable::gtable_add_rows(
            panel_table, max_height(strips$x$bottom), -2)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$x$bottom, -2, panel_pos_col$l, clip = "on",
            name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        } else {
          panel_table <- gtable::gtable_add_rows(
            panel_table, strip_padding, -1)
          panel_table <- gtable::gtable_add_rows(
            panel_table, max_height(strips$x$bottom), -1)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on",
            name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$x$top)) {
        if (inside_x || all(vapply(axes$x$top, is.zero, logical(1)))) {
          panel_table <- gtable::gtable_add_rows(
            panel_table, max_height(strips$x$top), 1)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$x$top, 2, panel_pos_col$l, clip = "on",
            name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        } else {
          panel_table <- gtable::gtable_add_rows(
            panel_table, strip_padding, 0)
          panel_table <- gtable::gtable_add_rows(
            panel_table, max_height(strips$x$top), 0)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on",
            name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        }
      }
    }
    panel_pos_rows <- panel_rows(panel_table)
    if (switch_y) {
      if (!is.null(strips$y$left)) {
        if (inside_y || all(vapply(axes$y$left, is.zero, logical(1)))) {
          panel_table <- gtable::gtable_add_cols(
            panel_table, max_width(strips$y$left), 1)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$y$left, panel_pos_rows$t, 2, clip = "on",
            name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        } else {
          panel_table <- gtable::gtable_add_cols(
            panel_table, strip_padding, 0)
          panel_table <- gtable::gtable_add_cols(
            panel_table, max_width(strips$y$left), 0)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on",
            name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$y$right)) {
        if (inside_y || all(vapply(axes$y$right, is.zero, logical(1)))) {
          panel_table <- gtable::gtable_add_cols(
            panel_table, max_width(strips$y$right), -2)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$y$right, panel_pos_rows$t, -2, clip = "on",
            name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        } else {
          panel_table <- gtable::gtable_add_cols(
            panel_table, strip_padding, -1)
          panel_table <- gtable::gtable_add_cols(
            panel_table, max_width(strips$y$right), -1)
          panel_table <- gtable::gtable_add_grob(
            panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on",
            name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        }
      }
    }
    panel_table
  },
  
  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  }
)

# Helpers -----------------------------------------------------------------

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(ggplot2:::unique0(x))
  }
}

censor_labels <- function(ranges, layout, labels) {
  if (labels$x && labels$y) {
    return(ranges)
  }
  draw <- matrix(
    TRUE, length(ranges), 4,
    dimnames = list(NULL, c("top", "bottom", "left", "right"))
  )
  
  if (!labels$x) {
    xmax <- stats::ave(layout$ROW, layout$COL, FUN = max)
    xmin <- stats::ave(layout$ROW, layout$COL, FUN = min)
    draw[which(layout$ROW != xmax), "bottom"] <- FALSE
    draw[which(layout$ROW != xmin), "top"] <- FALSE
  }
  if (!labels$y) {
    ymax <- stats::ave(layout$COL, layout$ROW, FUN = max)
    ymin <- stats::ave(layout$COL, layout$ROW, FUN = min)
    draw[which(layout$COL != ymax), "right"] <- FALSE
    draw[which(layout$COL != ymin), "left"] <- FALSE
  }
  for (i in seq_along(ranges)) {
    ranges[[i]]$draw_labels <- as.list(draw[i, ])
  }
  ranges
}
