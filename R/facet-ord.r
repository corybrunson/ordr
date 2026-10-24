#' @title Arrange biplots of all pairs of ordination coordinates
#'
#' @description Facet biplots using all ordered pairs of scaffolding coordinates
#'   in an ordination object.
#'
#' @details `facet_ord()` is intended to be added to a plot produced by
#'   [ggbiplot()], which stores the recovered ordination coordinates in the
#'   fortified data of each layer. Adding `facet_ord()` replaces the `x` and `y`
#'   mappings with internal panel-specific columns, so that the coordinates
#'   mapped in the original call are used only to set the coordinate system.
#'
#'   As in [ggplot2::facet_grid()], the `scales` and `space` parameters control
#'   the scales and sizes of the panels. Setting either parameter to a value
#'   also sets the other to the same value, unless both are set; if neither is
#'   set, both default to `"fixed"`. Setting `scales = "fixed"` obtains the same
#'   plot regardless of `space`. Pairing `scales = "free"` with `space =
#'   "fixed"` warps aspect ratios away from 1 and is discouraged though not
#'   warned against.
#'
#' @param coords Coordinates to include. Can be:
#'   \itemize{
#'     \item \code{NULL} (default): all coordinates
#'     \item A single integer: number of first coordinates to include (if exceeds
#'       dimension, a warning is issued and all are used)
#'     \item A character scalar: a single coordinate name
#'     \item A numeric vector: specific coordinate indices (1-based); error if any
#'       index exceeds the dimension
#'     \item A character vector: specific coordinate names; error if any name is
#'       not found exactly
#'   }
#' @param scales A character scalar indicating whether all panels share the same
#'   x and y coordinate scales (`"fixed"`) or whether the x and y scales are
#'   free to differ between panels (`"free"`). By default, `NULL`. Unlike
#'   [facet_grid()], no x- or y-specific values are available.
#' @param space A character scalar indicating whether all panels are the same
#'   size (`"fixed"`) or whether each panel is sized proportionally to the
#'   ranges of its coordinate scales (`"free"`). By default, `NULL`. Unlike
#'   [facet_grid()], no x- or y-specific values are available.
#' @return A `FacetOrd` ggproto object to be added to a [ggbiplot()].
#' @seealso [ggbiplot()], [ggplot2::facet_grid()]
#' @example inst/examples/ex-facet-ord-iris.r
#' @example inst/examples/ex-facet-ord-haireye.r
#' @export
facet_ord <- function(coords = NULL, scales = NULL, space = NULL) {
  if (!is.null(coords)) {
    validate_coords(coords, call = rlang::call2("facet_ord"))
  }
  if (is.null(scales) && is.null(space)) {
    scales <- space <- "fixed"
  } else if (is.null(scales) || is.null(space)) {
    given <- if (is.null(scales)) space else scales
    scales <- space <- match.arg(given, c("free", "fixed"))
  } else {
    scales <- match.arg(scales, c("free", "fixed"))
    space <- match.arg(space, c("free", "fixed"))
  }

  ggproto(
    NULL, FacetOrd,
    shrink = TRUE,
    params = list(
      coords = coords,
      scales = scales,
      space = space,
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

    if (! is.null(params$coords)) {
      if (length(params$coords) == 1L && is.numeric(params$coords)) {
        n <- params$coords
        if (n > length(coords)) {
          cli::cli_warn(c(
            "`coords = {n}` exceeds available dimensions ({length(coords)}).",
            i = "Using all {length(coords)} coordinates."
          ))
          n <- length(coords)
        }
        coords <- coords[seq_len(n)]
      } else if (is.numeric(params$coords)) {
        if (any(params$coords > length(coords))) {
          cli::cli_abort(c(
            "Coordinate indices exceed available dimensions.",
            x = "Maximum index is {length(coords)}, got {max(params$coords)}."
          ))
        }
        coords <- coords[params$coords]
      } else if (is.character(params$coords)) {
        missing <- setdiff(params$coords, coords)
        if (length(missing) > 0) {
          cli::cli_abort(c(
            "Coordinate name(s) not found: {.val {missing}}.",
            i = "Available: {.val {coords}}."
          ))
        }
        coords <- params$coords
      }
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

  draw_panels = function(
    self, panels, layout, x_scales = NULL,
    y_scales = NULL, ranges, coord, data = NULL,
    theme, params
  ) {
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
      cols = data_frame0(!!! labeller(col_vars)),
      rows = data_frame0(!!! labeller(row_vars))
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

validate_coords <- function(coords, call = rlang::caller_env()) {
  if (is.list(coords)) {
    cli::cli_abort("`coords` must be a vector, not a list.", call = call)
  }
  if (length(coords) == 1L) {
    if (is.numeric(coords)) {
      stopifnot(
        coords == round(coords),
        coords >= 1L
      )
    } else if (!is.character(coords)) {
      cli::cli_abort("`coords` must be numeric or character.", call = call)
    }
  } else {
    if (is.numeric(coords)) {
      stopifnot(
        all(coords == round(coords)),
        all(coords >= 1L)
      )
    } else if (!is.character(coords)) {
      cli::cli_abort("`coords` must be numeric or character.", call = call)
    }
  }
  invisible(NULL)
}
