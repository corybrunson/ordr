devtools::load_all()

(iris_pca <- ordinate(iris, cols = 1:4, model = ~ prcomp(., scale. = TRUE)))

iris_meta <- data.frame(
  Species = c("setosa", "versicolor", "virginica"),
  Colony = c(1L, 1L, 2L),
  Cytotype = c("diploid", "hexaploid", "tetraploid")
)
(iris_pca <- left_join_rows(iris_pca, iris_meta, by = "Species"))

tidy(iris_pca) %T>% print() %>%
  ggplot(aes(x = name, y = prop_var)) +
  geom_col() +
  labs(x = "", y = "Proportion of inertia") +
  ggtitle("PCA of Anderson's iris measurements",
          "Distribution of inertia")

ggbiplot(iris_pca, sec.axes = "cols", scale.factor = 2) +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_ellipse(aes(color = Species), alpha = .5, level = .99) +
  geom_cols_vector() +
  geom_cols_text_radiate(aes(label = name)) +
  expand_limits(y = c(-3.5, NA)) +
  ggtitle("PCA of Anderson's iris measurements",
          "99% confidence ellipses; variables use top & right axes")

ggbiplot(iris_pca, axis.type = "predictive", axis.percents = FALSE) +
  theme_biplot() +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_center(
    aes(color = Species, shape = Species),
    size = 5, alpha = .5, fun.data = mean_se
  ) +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  ggtitle("Predictive biplot of Anderson's iris measurements",
          "Project a marker onto an axis to approximate its measurement")
aggregate(iris[, 1:4], by = iris[, "Species", drop = FALSE], FUN = mean)

# pairs biplot?
# problem: need to be able to plot row and column elements separately
iris_pca |> 
  fortify(.matrix = "both") |> 
  GGally::ggpairs(
    columns = c("PC1", "PC2", "PC3", "PC4"),
    aes(color = Species, shape = Species)
  ) +
  theme_biplot()

# https://ggobi.github.io/ggally/articles/ggpairs.html
# https://github.com/ggobi/ggally/blob/master/R/ggpairs.R
# https://github.com/ggobi/ggally/issues/415

# unwrap `ggpairs()`
data = mtcars
mapping = NULL
columns = 1:3
title = NULL
upper = 
  list(continuous = "cor", combo = "box_no_facet",
       discrete = "count", na = "na")
lower = 
  list(continuous = "points", combo = "facethist",
       discrete = "facetbar", na = "na")
diag = 
  list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag")
params = NULL
xlab = NULL
ylab = NULL
axisLabels = c("show", "internal", "none")
columnLabels = colnames(data[columns])
labeller = "label_value"
switch = NULL
showStrips = NULL
legend = NULL
cardinality_threshold = 15
progress = NULL
proportions = NULL

isSharedData <- inherits(data, "SharedData")

data_ <- GGally:::fix_data(data)
data <- GGally:::fix_data_slim(data_, isSharedData)

if (
  !missing(mapping) && !is.list(mapping) &&
  missing(columns)
) {
  columns <- mapping
  mapping <- NULL
}
GGally:::stop_if_bad_mapping(mapping)

columns <-
  GGally:::fix_column_values(data, columns,
                             columnLabels, "columns", "columnLabels")

upper <- GGally:::check_and_set_ggpairs_defaults(
  "upper", upper,
  continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"
)
lower <- GGally:::check_and_set_ggpairs_defaults(
  "lower", lower,
  continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"
)
diag <- GGally:::check_and_set_ggpairs_defaults(
  "diag", diag,
  continuous = "densityDiag", discrete = "barDiag", na = "naDiag",
  isDiag = TRUE
)

axisLabels <- 
  GGally:::fix_axis_label_choice(axisLabels, c("show", "internal", "none"))

proportions <- GGally:::ggmatrix_proportions(proportions, data, columns)

# get plot type information
dataTypes <- GGally:::plot_types(data, columns, columns, allowDiag = TRUE)

# make internal labels on the diag axis
if (identical(axisLabels, "internal")) {
  dataTypes$plotType[dataTypes$posX == dataTypes$posY] <- "label"
}

ggpairsPlots <- lapply(seq_len(nrow(dataTypes)), function(i) {
  plotType <- dataTypes[i, "plotType"]
  
  posX <- dataTypes[i, "posX"]
  posY <- dataTypes[i, "posY"]
  xColName <- dataTypes[i, "xVar"]
  yColName <- dataTypes[i, "yVar"]
  
  if (posX > posY) {
    types <- upper
  } else if (posX < posY) {
    types <- lower
  } else {
    types <- diag
  }
  
  sectionAes <- GGally:::add_and_overwrite_aes(
    GGally:::add_and_overwrite_aes(
      aes(x = !!as.name(xColName), y = !!as.name(yColName)),
      mapping
    ),
    types$mapping
  )
  
  args <- list(types = types, sectionAes = sectionAes)
  if (plotType == "label") {
    args$label <- columnLabels[posX]
  }
  
  plot_fn <- GGally:::ggmatrix_plot_list(plotType)
  
  p <- do.call(plot_fn, args)
  
  return(p)
})

plotMatrix <- GGally::ggmatrix(
  plots = ggpairsPlots,
  byrow = TRUE,
  nrow = length(columns),
  ncol = length(columns),
  xAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
  yAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
  labeller = labeller,
  switch = switch,
  showStrips = showStrips,
  showXAxisPlotLabels = identical(axisLabels, "show"),
  showYAxisPlotLabels = identical(axisLabels, "show"),
  title = title,
  xlab = xlab,
  ylab = ylab,
  data = data_,
  gg = NULL,
  progress = progress,
  legend = legend,
  xProportions = proportions,
  yProportions = proportions
)

plotMatrix

# `ggmatrix()`

plotList <- list()
for (i in 1:6) {
  plotList[[i]] <- GGally::ggally_text(paste("Plot #", i, sep = ""))
}
pm <- GGally::ggmatrix(
  plotList,
  2, 3,
  c("A", "B", "C"),
  c("D", "E"),
  byrow = TRUE
)
pm

# iris_list <- matrix(data = list(), 4, 4)
iris_list <- list()
for (i in seq(4)) for (j in seq(4)) {
  if (i == j) {
    p <- GGally::ggally_text(paste("PC", i, sep = ""))
  } else {
    p <- ggbiplot(iris_pca, aes(x = !! i, y = !! j),
                  axis.type = "predictive", axis.percents = FALSE) +
      geom_rows_point(aes(color = Species, shape = Species)) +
      stat_rows_center(
        aes(color = Species, shape = Species),
        size = 5, alpha = .5, fun.data = mean_se
      ) +
      geom_cols_axis(aes(label = name, center = center, scale = scale)) +
      coord_cartesian()
  }
  # iris_list[[i, j]] <- p
  iris_list[[i + (j - 1) * 4]] <- p
}
iris_matrix <- GGally::ggmatrix(
  iris_list,
  4, 4,
  paste("PC", 1:4, sep = ""),
  paste("PC", 1:4, sep = ""),
  byrow = TRUE
)
iris_matrix

#' 1. `ggmatrix()` makes sense as a workhorse for `ggbipairs()`.
#' 2. Plot layers need to be revised to work under `CoordCartesian`.
#' 3. It would be ideal if this could be done using `FacetGrid`.

# pivot + facet?
# pairs biplot?
# problem: need to be able to plot row and column elements separately
iris_pca |> 
  fortify(.matrix = "both") |> 
  # NOTE: Ensure that '.id' is not already a column name.
  dplyr::mutate(.id = dplyr::row_number()) |> 
  tidyr::pivot_longer(
    # NOTE: Determine axis prefix from ordination object.
    cols = starts_with("PC"),
    # NOTE: Ensure that '.axis' and '.value' are not already column names.
    names_to = ".axis", values_to = ".value"
  ) |>
  print() -> iris_pca_long
iris_pca_long |> 
  # NOTE: Creates all pairings, with 1D biplots along diagnoal.
  dplyr::full_join(
    iris_pca_long, by = setdiff(names(iris_pca_long), c(".axis", ".value")),
    relationship = "many-to-many", suffix = c("_x", "_y")
  ) |> 
  ggbiplot(aes(x = .value_x, y = .value_y)) +
  # NOTE: `coord_fixed()` doesn't support free scales.
  facet_grid(rows = vars(.axis_y), cols = vars(.axis_x)) +
  theme() ->
  iris_pca_pairs
iris_pca_pairs +
  geom_abline(slope = 1) +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_center(
    aes(color = Species, shape = Species),
    size = 5, alpha = .5, fun.data = mean_se
  ) +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  # ERROR: `layout$panel_params[[data$PANEL[1]]]`
  geom_origin() +
  geom_unit_circle() +
  # theme_biplot() +
  labs(x = NULL, y = NULL)

# QUESTION: Can this be accomplished inside a new `Facet*`, e.g. that can then
# be overridden by `+ facet_null()` to retrieve the usual biplot? It would need
# to use `ord_aes()`, maybe with an additional `num_axes` parameter.

FacetPairs <- ggproto(
  "FacetPairs", FacetGrid,
  
  compute_layout = function(self, data, params) {
    
    # list of data frames: [[1]] plot data, [[2]] individual layer data
    print("FacetPairs$compute_layout(data = ?)")
    print(data)
    print("FacetPairs$compute_layout(params = ?)")
    print(params)
    
    # NOTE: `data` is fortified data,
    # so internally pass coordinates to `params`
    # (either `x` and `y` or all `..coord[0-9]+`)
    # and throw an error if these cannot be identified
    # (since the user should be passing an ordination object)
    
    # use `ord_aes()` output if provided, otherwise `x` and `y` pairs
    dims <- recover_coord(data)
    # restrict to explicitly specified dimensions, if any
    if (! is.null(params$dims)) {
      if (is.numeric(params$dims)) {
        dims <- dims[intersect(seq_along(dims), params$dims)]
      } else if (is.character(params$dims)) {
        dims <- dims[match(params$dims, dims)]
      } else {
        warning("`dims` is neither numeric nor character and will be ignored.")
      }
    }
    
    
    
    rows <- params$rows
    cols <- params$cols
    ggplot2:::check_facet_vars(names(rows), names(cols),
                               name = snake_class(self))
    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      cli::cli_abort(
        c(
          "Faceting variables can only appear in {.arg rows} or {.arg cols}.\n", 
          i = "Duplicated variables: {.val {dups}}"
        ),
        call = call2(snake_class(self))
      )
    }
    base_rows <- combine_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
      base_rows[] <- lapply(base_rows, rev_order)
    }
    base_cols <- combine_vars(data, params$plot_env, cols, drop = params$drop)
    base <- ggplot2:::df.grid(base_rows, base_cols)
    if (nrow(base) == 0) {
      res <- ggplot2:::data_frame0(PANEL = factor(1L), ROW = 1L, COL = 1L, 
                                   SCALE_X = 1L, SCALE_Y = 1L)
      print(res)
      return(res)
    }
    base <- ggplot2:::reshape_add_margins(base, list(names(rows), names(cols)), 
                                          params$margins)
    base <- ggplot2:::unique0(base)
    panel <- ggplot2:::id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))
    rows <- if (!length(names(rows))) rep(1L, length(panel)) else
      ggplot2:::id(base[names(rows)], drop = TRUE)
    cols <- if (!length(names(cols))) rep(1L, length(panel)) else
      ggplot2:::id(base[names(cols)], drop = TRUE)
    panels <- ggplot2:::data_frame0(PANEL = panel, ROW = rows, COL = cols, base)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL
    panels$SCALE_X <- if (params$free$x) panels$COL else 1L
    panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L
    print(panels)
    panels
  }
)

facet_pairs <- function(dims = NULL,
                        # `facet_grid()`
                        rows = NULL, cols = NULL, scales = "fixed",
                        space = "fixed", shrink = TRUE,
                        labeller = "label_value", as.table = TRUE,
                        switch = NULL, drop = TRUE, margins = FALSE,
                        axes = "margins", axis.labels = "all",
                        facets = rlang:::deprecated()) {
  # `facets` is deprecated and renamed to `rows`
  if (lifecycle::is_present(facets)) {
    deprecate_warn0("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
    rows <- facets
  }
  
  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  
  scales <- rlang::arg_match0(scales %||%
                                "fixed", c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  
  space <- rlang::arg_match0(space %||%
                               "fixed", c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )
  
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
  
  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  
  # Check for deprecated labellers
  labeller <- ggplot2:::check_labeller(labeller)
  
  ggproto(
    NULL, FacetPairs,
    shrink = shrink,
    params = list(
      dims = dims,
      # `facet_grid()`
      rows = facets_list$rows, cols = facets_list$cols, margins = margins,
      free = free, space_free = space_free, labeller = labeller,
      as.table = as.table, switch = switch, drop = drop,
      draw_axes = draw_axes, axis_labels = axis_labels
    )
  )
}

ggplot(mtcars[seq(6), ], aes(x = mpg, y = hp)) +
  facet_pairs(cols = vars(cyl)) +
  geom_point()
