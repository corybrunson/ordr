#' @title Facet a biplot into all pairs of ordination coordinates
#'
#' @description This layer faceting function draws a biplot of every ordered
#'   pair of ordination coordinates, with one panel for each pair. The panels
#'   are drawn on a grid, with a coordinate name labeling each column and row of
#'   the grid. As in [facet_grid()], the `scales` and `space` parameters control
#'   whether the coordinate scales and the sizes of the panels are the same
#'   across all panels or free to vary between panels.
#'
#' @details `facet_ord()` is intended to be added to a plot produced by
#'   [ggbiplot()], which stores the recovered ordination coordinates in the
#'   fortified data of each layer. Adding `facet_ord()` replaces the `x` and `y`
#'   mappings with internal panel-specific columns, so that the coordinates
#'   mapped in the original call are used only to set the coordinate system.
#'
#'   The `scales` and `space` parameters control the scales and sizes of the
#'   panels, as in [facet_grid()]. With `scales = "free"`, the x and y scales
#'   of each panel are determined by its own coordinates, while with
#'   `scales = "fixed"`, all panels share the same x and y scales. With
#'   `space = "free"`, each panel is sized so that the units of its x and y
#'   coordinates are rendered at the same physical length, preserving the
#'   geometry of the ordination; with `space = "fixed"`, all panels are the
#'   same size.
#'
#' @param num_coord The number of ordination coordinates to include. A positive
#'   integer; by default, all recovered coordinates are included.
#' @param scales A character scalar indicating whether all panels share the same
#'   x and y coordinate scales (`"fixed"`) or whether the x and y scales are
#'   free to differ between panels (`"free"`). The default is `"fixed"`. Unlike
#'   [facet_grid()], no x- or y-specific values are available.
#' @param space A character scalar indicating whether all panels are the same
#'   size (`"fixed"`) or whether each panel is sized proportionally to the
#'   ranges of its coordinate scales (`"free"`). The default is `"fixed"`.
#'   Unlike [facet_grid()], no x- or y-specific values are available.
#' @example inst/examples/ex-facet-ord-iris.r
#' @export
facet_ord <- function(num_coord = NULL, scales = "fixed", space = "fixed") {
  if (! is.null(num_coord)) {
    stopifnot(
      is.numeric(num_coord),
      length(num_coord) == 1L,
      num_coord == round(num_coord),
      num_coord >= 1L
    )
    num_coord <- as.integer(num_coord)
  }
  scales <- match.arg(scales, c("free", "fixed"))
  space <- match.arg(space, c("free", "fixed"))

  ggproto(
    NULL, FacetOrd,
    shrink = TRUE,
    params = list(
      num_coord = num_coord,
      scales = scales,
      space = space,
      # `facet_grid()` parameters
      margins = FALSE,
      free = list(x = scales == "free", y = scales == "free"),
      space_free = list(x = space == "free", y = space == "free"),
      labeller = label_value,
      as.table = TRUE,
      switch = NULL,
      drop = TRUE,
      draw_axes = list(x = FALSE, y = FALSE),
      axis_labels = list(x = TRUE, y = TRUE)
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetOrd <- ggproto(
  "FacetOrd", FacetGrid,

  setup_params = function(self, data, params) {
    params <- ggproto_parent(FacetGrid, self)$setup_params(data, params)

    # recover the shared coordinates from the fortified data
    coords <- NULL
    for (d in data) {
      if (is.null(d)) next
      if (! is.null(attr(d, "coordinates"))) {
        coords <- attr(d, "coordinates")
        break
      }
    }
    # fall back to numeric columns shared by both matrix factors
    if (is.null(coords)) {
      for (d in data) {
        if (is.null(d) || ! ".matrix" %in% names(d)) next
        common <- intersect(
          names(d[d$.matrix == "rows", ]),
          names(d[d$.matrix == "cols", ])
        )
        coords <- common[vapply(
          common, function(nm) is.numeric(d[[nm]]), logical(1)
        )]
        break
      }
    }
    if (length(coords) < 1L) {
      cli::cli_abort(
        c(
          "`{snake_class(self)}` requires shared coordinates.",
          i = "Pass a {.cls tbl_ord} object to {.fn ggbiplot}."
        ),
        call = rlang::call2(snake_class(self))
      )
    }

    # restrict to the first `num_coord` coordinates
    if (! is.null(params$num_coord)) {
      coords <- coords[seq_len(min(params$num_coord, length(coords)))]
    }
    params$coords <- coords

    params
  },

  compute_layout = function(self, data, params) {
    coords <- params$coords
    k <- length(coords)

    # all ordered pairs of coordinates
    grid <- expand.grid(
      x = seq_len(k), y = seq_len(k),
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    panels <- data_frame0(
      x = coords[grid$x],
      y = coords[grid$y]
    )
    panels$PANEL <- factor(seq_len(nrow(panels)))
    panels$ROW <- grid$y
    panels$COL <- grid$x
    panels$SCALE_X <- if (params$free$x) grid$x else 1L
    panels$SCALE_Y <- if (params$free$y) grid$y else 1L
    panels
  },

  map_data = function(data, layout, params) {
    if (empty(data)) {
      return(data)
    }
    coords <- params$coords
    missing <- setdiff(coords, names(data))
    x_pos <- match(layout$x, coords)
    y_pos <- match(layout$y, coords)
    n <- nrow(data)
    npanel <- nrow(layout)
    data <- data[rep.int(seq_len(n), npanel), , drop = FALSE]
    data$PANEL <- rep(layout$PANEL, each = n)
    # assign each row's coordinates to its panel
    data$.panel_x <- unlist(lapply(seq_len(npanel), function(i) {
      nm <- coords[[x_pos[[i]]]]
      if (nm %in% missing) rep(NA_real_, n)
      else data[[nm]][seq_len(n) + (i - 1L) * n]
    }), use.names = FALSE)
    data$.panel_y <- unlist(lapply(seq_len(npanel), function(i) {
      nm <- coords[[y_pos[[i]]]]
      if (nm %in% missing) rep(NA_real_, n)
      else data[[nm]][seq_len(n) + (i - 1L) * n]
    }), use.names = FALSE)
    data
  },

  draw_panels = function(self, panels, layout, x_scales = NULL,
                         y_scales = NULL, ranges, coord, data = NULL,
                         theme, params) {
    # a fixed-ratio coordinate system cannot be combined with free scales or
    # space freely, so the default checks are bypassed; the panels are always
    # sized so that equal coordinate differences have equal physical length
    table <- self$init_gtable(
      panels, layout, theme, ranges, params, aspect_ratio = 1
    )
    table <- self$attach_axes(table, layout, ranges, coord, theme, params)
    self$attach_strips(table, layout, params, theme)
  },

  format_strip_labels = function(self, layout, params) {
    labeller <- match.fun(params$labeller)
    cols <- unique0(layout$x)
    rows <- unique0(layout$y)
    col_vars <- data_frame0(x = factor(cols, levels = cols))
    row_vars <- data_frame0(y = factor(rows, levels = rows))
    attr(col_vars, "type") <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type") <- "rows"
    attr(row_vars, "facet") <- "grid"
    list(
      cols = data_frame0(!!!labeller(col_vars)),
      rows = data_frame0(!!!labeller(row_vars))
    )
  }
)

#' @export
ggplot_add.FacetOrd <- function(object, plot, object_name) {
  # manage the x and y coordinates via the facet
  plot$mapping$x <- rlang::sym(".panel_x")
  plot$mapping$y <- rlang::sym(".panel_y")
  # suppress the axis titles, which are replaced by the facet strips; setting
  # them to `NULL` explicitly prevents them from being re-derived from the
  # mapping, while still allowing overrides via `labs()`
  plot <- plot + labs(x = NULL, y = NULL)
  # set the facet, as the default `update_ggplot()` method would
  plot$facet <- object
  plot
}

data_frame0 <- getFromNamespace("data_frame0", "ggplot2")
empty <- getFromNamespace("empty", "ggplot2")
unique0 <- getFromNamespace("unique0", "ggplot2")
