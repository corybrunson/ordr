#' Facet by different data columns
#'
#' The `facet_matrix()` facet allows you to put different data columns into
#' different rows and columns in a grid of panels. If the same data columns are
#' present in both the rows and the columns of the grid, and used together with
#' [ggplot2::geom_point()] it is also known as a scatterplot matrix, and if
#' other geoms are used it is sometimes referred to as a pairs plot.
#' `facet_matrix` is so flexible that these types are simply a subset of its
#' capabilities, as any combination of data columns can be plotted against each
#' other using any type of geom. Layers should use the `.panel_x` and `.panel_y`
#' placeholders to map aesthetics to, in order to access the row and column
#' data.
#'
#' @param rows,cols A specification of the data columns to put in the rows and
#' columns of the facet grid. They are specified using the [ggplot2::vars()]
#' function wherein you can use standard tidyselect syntax as known from e.g.
#' `dplyr::select()`. These data values will be made available to the different
#' layers through the `.panel_x` and `.panel_y` variables.
#' @inheritParams ggplot2::facet_grid
#' @param flip.rows Should the order of the rows be reversed so that, if the
#' rows and columns are equal, the diagonal goes from bottom-left to top-right
#' instead of top-left to bottom-right.
#' @param alternate.axes Should axes be drawn at alternating positions.
#' @param layer.lower,layer.diag,layer.upper Specification for where each layer
#' should appear. The default (`NULL`) will allow any layer that has not been
#' specified directly to appear at that position. Putting e.g. `layer.diag = 2`
#' will make the second layer appear on the diagonal as well as remove that
#' layer from any position that has `NULL`. Using `TRUE` will put all layers at
#' that position, and using `FALSE` will conversely remove all layers. These
#' settings will only have an effect if the grid is symmetric.
#' @param grid.y.diag Should the y grid be removed from the diagonal? In certain
#' situations the diagonal are used to plot the distribution of the column data
#' and will thus not use the y-scale. Removing the y gridlines can indicate
#' this.
#'
#' @note Due to the special nature of this faceting it slightly breaks the
#' ggplot2 API, in that any positional scale settings are ignored. This is
#' because each row and column in the grid will potentially have very different
#' scale types and it is not currently possible to have multiple different scale
#' specifications in the same plot object.
#'
#' @seealso [geom_autopoint], [geom_autohistogram], [geom_autodensity], and
#' [position_auto] for geoms and positions that adapts to different positional
#' scale types
#'
#' @export
facet_pairs <- function(
    dims = NULL, shrink = TRUE, switch = NULL,
    labeller = "label_value",
    scales = "fixed", space = "fixed",
    flip.rows = FALSE,
    alternate.axes = FALSE,
    layer.lower = NULL, layer.diag = NULL, layer.upper = NULL,
    grid.y.diag = TRUE
) {
  # if (! rlang::is_quosures(dims)) dims <- quos(dims)
  if (is.null(dims)) dims <- seq(2L)
  
  labeller <- utils::getFromNamespace('check_labeller', 'ggplot2')(labeller)
  
  scales <- rlang::arg_match0(scales %||% "fixed", c("fixed", "free"))
  free <- list(x = scales == "free", y = scales == "free")
  
  space <- rlang::arg_match0(space %||% "fixed", c("fixed", "free"))
  space_free <- list(x = space == "free", y = space == "free")
  
  ggproto(
    NULL, FacetPairs,
    shrink = shrink,
    params = list(
      # dims = quos(row_data = row_data),
      # dim_vars = dims,
      dims = dims,
      switch = switch,
      labeller = labeller,
      free = free, space_free = space_free,
      margins = FALSE,
      as.table = !flip.rows, drop = TRUE,
      alternate.axes = alternate.axes,
      layer.lower = layer.lower, layer.diag = layer.diag,
      layer.upper = layer.upper,
      grid.y.diag = grid.y.diag
    )
  )
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom tidyselect eval_select
#' @export
FacetPairs <- ggproto(
  "FacetPairs", FacetGrid,
  
  # `data` is a list of data frames, one for each layer
  setup_data = function(data, params) {
    save(data, params,
         file = here::here("ignore/facet-pairs-setup-data.rda"))
    # load(here::here("ignore/facet-pairs-setup-data.rda"))
    
    lapply(seq_along(data), function(i) {
      d <- data[[i]]
      d$.layer_index <- i - 1
      d
    })
  },
  
  # `data` is a list of data frames, one for each layer
  setup_params = function(data, params) {
    save(data, params,
         file = here::here("ignore/facet-pairs-setup-params.rda"))
    # load(here::here("ignore/facet-pairs-setup-params.rda"))
    
    # CURRENT
    # dims <- lapply(data, function(d) {
    #   names(eval_select(quo(c(!!!params$dim_vars)), d))
    # })
    # dims <- ggplot2:::unique0(unlist(dims))
    # possibility exists that alternate data frames are passed to `data`;
    # in this case, is it OK for this facet to break?
    # no: want to force consistent data to be passed, so check that names agree
    dims <- lapply(data, function(d) {
      names(d)[params$dims]
    })
    dims <- unique(dims)
    if (length(dims) > 1L) 
      stop("`dims` do not agree across layer data frames.")
    dims <- ggplot2:::unique0(unlist(dims))
    # check that all variables follow a naming pattern, e.g. `..coord1`, etc.
    if (length(unique(sub("[0-9]+$", "", dims))) > 1L)
      stop(
        "`dims` are not all ordination dimensions:\n",
        paste0(dims, collapse = ", ")
      )
    params$dims <- dims
    
    if (length(dims) == 0) {
      cli::cli_abort('{.arg dims} must select valid data columns')
    }
    # {ordr}
    # rows <- dims; cols <- dims
    # if (! params$as.table) rows <- rev(rows)
    # params$row_vars <- rows
    # params$col_vars <- cols
    params$row_vars <- if (params$as.table) dims else rev(dims)
    params$col_vars <- dims
    # Horrible hack - don't judge
    # plot_env <- get('plot_env', rlang::caller_env(2)) 
    # save(plot_env,
    #      file = here::here("ignore/facet-pairs-setup-params-plot-env.rda"))
    # load(here::here("ignore/facet-pairs-setup-params-plot-env.rda"))
    
    # {ordr}
    # params$row_scales <- 
    #   create_pos_scales(rows, data, plot_env, 'y', params$alternate.axes)
    # params$col_scales <- 
    #   create_pos_scales(cols, data, plot_env, 'x', params$alternate.axes)
    create_pos_scales_ord <- function(dim) {
      poses <- if (dim == "x") c("bottom", "top") else c("left", "right")
      pos <- if (params$alternate.axes) poses[((i - 1) %% 2) + 1] else poses[1]
      d <- lapply(data, .subset2, dims[[1L]])
      # TODO: un-loop this
      scales <- replicate(length(params$dims), {
        type <- paste0("scale_", dim, "_continuous")
        scale_fun <- get(type, asNamespace("ggplot2"), mode = "function")
        scale <- scale_fun(name = NULL, position = pos)
        lapply(d, scale$train)
        scale
      })
      names(scales) <- dims
      scales
    }
    params$row_scales <- create_pos_scales_ord("y")
    params$col_scales <- create_pos_scales_ord("x")
    
    n_layers <- length(data) - 1
    params$layer_pos <- assign_layers(
      n_layers,
      lower = params$layer.lower,
      diagonal = params$layer.diag,
      upper = params$layer.upper
    )
    
    params
  },
  
  compute_layout = function(data, params) {
    save(data, params,
         file = here::here("ignore/facet-pairs-compute-layout.rda"))
    # load(here::here("ignore/facet-pairs-compute-layout.rda"))
    
    layout <- expand.grid(
      col_data = params$col_vars,
      row_data = params$row_vars,
      stringsAsFactors = FALSE
    )
    layout$ROW <- match(layout$row_data, params$row_vars)
    layout$COL <- match(layout$col_data, params$col_vars)
    layout$PANEL <- factor(seq_len(nrow(layout)))
    layout$SCALE_X <- layout$COL
    layout$SCALE_Y <- layout$ROW
    
    mat_ind <- matrix(
      seq_len(nrow(layout)),
      length(params$row_vars),
      length(params$col_vars),
      byrow = TRUE
    )
    if (!params$as.table) mat_ind <- mat_ind[rev(seq_along(params$row_vars)), ]
    layout$panel_pos <- 'lower'
    layout$panel_pos[diag(mat_ind)] <- 'diagonal'
    layout$panel_pos[mat_ind[upper.tri(mat_ind)]] <- 'upper'
    
    layout
  },
  
  # create the faceted data for each layer (`data$.layer_index`)
  map_data = function(data, layout, params) {
    save(data, layout, params,
         file = here::here("ignore/facet-pairs-map-data.rda"))
    # load(here::here("ignore/facet-pairs-map-data.rda"))
    
    # `data$.layer_index` must be constant
    layer_pos <- params$layer_pos[[data$.layer_index[1]]]
    data <- lapply(seq_len(nrow(layout)), function(i) {
      row <- layout$row_data[i]
      col <- layout$col_data[i]
      # {ordr}: union of ranges will be range along first dimension
      scale_row <- if (params$free$y) layout$row_data[i] else layout$row_data[1]
      scale_col <- if (params$free$x) layout$col_data[i] else layout$col_data[1]
      
      placeholder <- cbind(data[0, ], PANEL = layout$PANEL[0],
                           .panel_x = numeric(), .panel_y = numeric())
      if (!all(c(row, col) %in% names(data))) return(placeholder)
      if (!layout$panel_pos[i] %in% layer_pos) return(placeholder)
      
      data$PANEL <- layout$PANEL[i]
      # {ordr}
      data$.panel_x <- params$col_scales[[scale_col]]$map(data[[col]])
      data$.panel_y <- params$row_scales[[scale_row]]$map(data[[row]])
      if (packageVersion('ggplot2') <= '3.3.6') {
        if (inherits(data$.panel_x, 'mapped_discrete')) {
          data$.panel_x <- unclass(data$.panel_x)
        }
        if (inherits(data$.panel_y, 'mapped_discrete')) {
          data$.panel_y <- unclass(data$.panel_y)
        }
      }
      data
    })
    vctrs::vec_rbind(!!!data)
  },
  
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    save(layout, x_scale, y_scale, params,
         file = here::here("ignore/facet-pairs-init-scales.rda"))
    # load(here::here("ignore/facet-pairs-init-scales.rda"))
    
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- params$col_scales
    }
    if (!is.null(y_scale)) {
      scales$y <- params$row_scales
    }
    scales
  },
  
  draw_panels = function(
    self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params
  ) {
    save(self, panels, layout, x_scales, y_scales, ranges, coord,
         data, theme, params,
         file = here::here("ignore/facet-pairs-draw-panels.rda"))
    # load(here::here("ignore/facet-pairs-draw-panels.rda"))
    
    if (!params$grid.y.diag) {
      panels[layout$panel_pos == 'diagonal'] <- lapply(
        panels[layout$panel_pos == 'diagonal'],
        function(panel) {
          grill <- grep('^grill', names(panel$children))
          y_grid <- grep(
            '^panel\\.grid\\.(major\\.y)|(minor\\.y)',
            names(panel$children[[grill]]$children)
          )
          panel$children[[grill]]$children[y_grid] <-
            rep(list(zeroGrob()), length(y_grid))
          panel
        }
      )
    }
    ggproto_parent(FacetGrid, self)$draw_panels(
      panels, layout, x_scales, y_scales, ranges, coord, data, theme, params
    )
  }
)

create_pos_scales <- function(vars, data, env, dim = 'x', alternate = FALSE) {
  positions <- if (dim == 'x') c('bottom', 'top') else c('left', 'right')
  scales <- lapply(seq_along(vars), function(i) {
    # {ordr} HACK
    # var <- vars[i]
    var <- vars[1L]
    pos <- if (alternate) positions[((i - 1) %% 2) + 1] else positions[1]
    d <- lapply(data, .subset2, var)
    d <- d[lengths(d) != 0]
    # {ordr} scale will always be "continuous"
    type <- paste0('scale_', dim, '_', scale_type(d[[1]]))
    scales <- lapply(type, function(t) {
      if (exists(t, env, mode = 'function')) {
        scale <- get(t, env, mode = 'function')
      } else if (exists(t, asNamespace('ggplot2'), mode = 'function')) {
        scale <- get(t, asNamespace('ggplot2'), mode = 'function')
      # } else if (exists(t, env, mode = 'function')) {
      #   scale <- get(t, env, mode = 'function')
      } else if (exists(t, asNamespace('ggforce'), mode = 'function')) {
        scale <- get(t, asNamespace('ggforce'), mode = 'function')
      } else {
        NULL
      }
    })
    scales <- scales[lengths(scales) != 0]
    if (length(scales) == 0) {
      cli::cli_abort('Unable to pick a scale for {.col {var}}')
    }
    scale <- scales[[1]](name = NULL, position = pos)
    lapply(d, scale$train)
    scale
  })
  names(scales) <- vars
  scales
}

assign_layers <- function(n_layers, ...) {
  specs <- list(...)
  layers <- seq_len(n_layers)
  specs <- lapply(specs, function(spec) {
    if (is.null(spec)) return(spec)
    if (is.logical(spec)) {
      if (spec) layers else integer()
    } else {
      layers[spec]
    }
  })
  
  specified_layers <- sort(ggplot2:::unique0(unlist(specs)))
  specified_layers <- layers %in% specified_layers
  
  specs <- lapply(specs, function(spec) {
    if (is.null(spec)) {
      layers[!specified_layers]
    } else {
      spec
    }
  })
  split(
    unlist(lapply(
      names(specs),
      function(name) rlang::rep_along(specs[[name]], name)
    )),
    factor(unlist(specs), levels = layers)
  )
}

utils::globalVariables(c('.panel_x', '.panel_y', 'row_data', 'col_data'))
