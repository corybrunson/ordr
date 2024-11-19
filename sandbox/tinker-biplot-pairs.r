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
  )

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

# from {gtable}

library(grid)
a <- rectGrob(gp = gpar(fill = "red"))
b <- circleGrob()
c <- linesGrob()

row <- matrix(list(a, b, c), nrow = 1)
col <- matrix(list(a, b, c), ncol = 1)
mat <- matrix(list(a, b, c, nullGrob()), nrow = 2)

gt <- 
  gtable::gtable_matrix("demo", row, unit(c(1, 1, 1), "null"), unit(1, "null"))
class(gt)

# Can specify z ordering
z <- matrix(c(3, 1, 2, 4), nrow = 2)
gt2 <- 
  gtable::gtable_matrix("demo", mat,
                        unit(c(1, 1), "null"), unit(c(1, 1), "null"), z = z)

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
  ) ->
  iris_pca_pairs
iris_pca_pairs |> 
  # dplyr::distinct(.axis_x, .axis_y)
  ggbiplot(aes(x = .value_x, y = .value_y)) +
  # NOTE: `coord_fixed()` doesn't support free scales.
  # facet_grid(rows = vars(.axis_y), cols = vars(.axis_x)) +
  facet_ord(rows = vars(.axis_y), cols = vars(.axis_x)) +
  theme() +
  geom_abline(slope = 1) +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_center(
    aes(color = Species, shape = Species),
    size = 5, alpha = .5, fun.data = mean_se
  ) +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  geom_origin() +
  geom_unit_circle() +
  # theme_biplot() +
  labs(x = NULL, y = NULL)

# QUESTION: Can this be accomplished inside a new `Facet*`, e.g. that can then
# be overridden by `+ facet_null()` to retrieve the usual biplot? It would need
# to use `ord_aes()`, maybe with an additional `num_axes` parameter.

# using {ggforce}

# major problem: cannot adjust `scales` and `space` as in `facet_grid()`;
# `scales` would allow forcing aspect ratio = 1 while `space` would allow
# shrinking of windows for later dimensions
iris_pca |> 
  ggbiplot(sec.axes = "cols", scale.factor = 2,
           aes(x = .panel_x, y = .panel_y,
               color = Species, fill = Species, linetype = name)) +
  # legend is screwed up
  geom_rows_point() +
  geom_cols_vector() +
  # cannot handle `coord_equal()`
  coord_cartesian() +
  # preserves scales based on 2 variables at a time
  ggforce::geom_autodensity() +
  # geom_density2d() +
  ggforce::facet_matrix(
    vars(tidyselect::starts_with("PC")),
    # `shrink` doesn't seem to matter
    shrink = FALSE,
    # must remember order of layers
    layer.lower = c(1, 2), layer.diag = 3, layer.upper = 1,
    grid.y.diag = FALSE
  )

# could be a template on which to build `FacetPairs`, or maybe `FacetOrd`

# experimental `FacetPairs` adapted from `FacetMatrix`
iris_pca |> 
  ggbiplot(
    # sec.axes = "cols", scale.factor = 2,
    # TODO: enable `ord_aes()` here;
    # `facet_pairs()` should not depend on `.panel_*` hence break when removed
    aes(
      x = .panel_x, y = .panel_y,
      color = Species, fill = Species, linetype = name
    )
    # NOTE: no need to do this since `dims` can handle any number of dimensions
    # ord_aes(iris_pca, color = Species, fill = Species, linetype = name)
  ) +
  # FIXME: legend screws up when `geom_autodensity()` is used
  geom_rows_point() +
  geom_cols_vector() +
  # cannot combine `coord_equal()` with `scales = "free"`
  # coord_cartesian() +
  # preserves scales based on 2 variables at a time
  # ggforce::geom_autodensity() +
  # geom_density2d() +
  facet_pairs(
    # TODO: keep `dims`, but apply to `aes(x = 1, y = 2)` or `ord_aes()`
    # CURRENT
    # dims = vars(tidyselect::starts_with("PC")),
    dims = 1:3,
    # FIXME: want to have 2 options, both with same resolution across all axes:
    # 1. fixed-size panels, which leaves empty space for later dimensions
    #    (below settings fail to enforce constant resolution):
    # scales = "fixed", space = "fixed"
    # scales = "free", space = "fixed"
    # 2. variable-size panels, to minimize empty space:
    # scales = "free", space = "free"
    # actual experiment:
    scales = "fixed", space = "fixed",
    # must remember order of layers
    layer.lower = c(1, 2), layer.diag = 3, layer.upper = 1,
    grid.y.diag = FALSE
  )
