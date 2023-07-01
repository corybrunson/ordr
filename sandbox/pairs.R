# https://github.com/corybrunson/ordr/issues/29

# `GGally::ggpairs()`
function (
    data,
    mapping = NULL,
    columns = 1:ncol(data),
    title = NULL, 
    upper = list(
      continuous = "cor",
      combo = "box_no_facet",
      discrete = "count",
      na = "na"
    ),
    lower = list(
      continuous = "points",
      combo = "facethist",
      discrete = "facetbar",
      na = "na"
    ), 
    diag = list(
      continuous = "densityDiag",
      discrete = "barDiag", 
      na = "naDiag"
    ),
    params = NULL,
    ...,
    xlab = NULL, ylab = NULL, 
    axisLabels = c("show", "internal", "none"),
    columnLabels = colnames(data[columns]), 
    labeller = "label_value",
    switch = NULL,
    showStrips = NULL, 
    legend = NULL,
    cardinality_threshold = 15,
    progress = NULL, 
    proportions = NULL,
    legends = stop("deprecated")
) {
  warn_deprecated(!missing(legends), "legends")
  warn_if_args_exist(list(...))
  stop_if_params_exist(params)
  isSharedData <- inherits(data, "SharedData")
  data_ <- fix_data(data)
  data <- fix_data_slim(data_, isSharedData)
  if (!missing(mapping) & !is.list(mapping) & missing(columns)) {
    columns <- mapping
    mapping <- NULL
  }
  stop_if_bad_mapping(mapping)
  columns <- fix_column_values(
    data, columns, columnLabels, "columns", "columnLabels"
  )
  stop_if_high_cardinality(data, columns, cardinality_threshold)
  upper <- check_and_set_ggpairs_defaults(
    "upper", upper, continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"
  )
  lower <- check_and_set_ggpairs_defaults(
    "lower", lower, continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"
  )
  diag <- check_and_set_ggpairs_defaults(
    "diag", diag, continuous = "densityDiag", discrete = "barDiag", na = "naDiag", isDiag = TRUE
  )
  axisLabels <- fix_axis_label_choice(
    axisLabels, c("show", "internal", "none")
  )
  proportions <- ggmatrix_proportions(proportions, data, columns)
  dataTypes <- plot_types(data, columns, columns, allowDiag = TRUE)
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
    }
    else if (posX < posY) {
      types <- lower
    }
    else {
      types <- diag
    }
    sectionAes <- add_and_overwrite_aes(add_and_overwrite_aes(aes_(
      x = as.name(xColName), 
      y = as.name(yColName)
    ), mapping), types$mapping)
    args <- list(
      types = types, sectionAes = sectionAes
    )
    if (plotType == "label") {
      args$label <- columnLabels[posX]
    }
    plot_fn <- ggmatrix_plot_list(plotType)
    p <- do.call(plot_fn, args)
    return(p)
  })
  plotMatrix <- ggmatrix(
    plots = ggpairsPlots, byrow = TRUE, 
    nrow = length(columns), ncol = length(columns),
    xAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
    yAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
    labeller = labeller, switch = switch, 
    showStrips = showStrips,
    showXAxisPlotLabels = identical(axisLabels, "show"),
    showYAxisPlotLabels = identical(axisLabels, "show"),
    title = title, xlab = xlab, ylab = ylab, 
    data = data_, gg = NULL, progress = progress, legend = legend, 
    xProportions = proportions, yProportions = proportions
  )
  plotMatrix
}

# from examples
data <- GGally::flea
mapping <- NULL
columns <- 2:4
upper <- list(
  continuous = "cor",
  combo = "box_no_facet",
  discrete = "count",
  na = "na"
)
lower <- list(
  continuous = "points",
  combo = "facethist",
  discrete = "facetbar",
  na = "na"
)
diag <- list(
  continuous = "densityDiag",
  discrete = "barDiag", 
  na = "naDiag"
)
axisLabels <- c("show", "internal", "none")
columnLabels <- colnames(data[columns])
cardinality_threshold <- 6L
proportions <- NULL

# from source code
# data_ <- GGally:::fix_data(data)
# data <- GGally:::fix_data_slim(data_, isSharedData)
# if (!missing(mapping) & !is.list(mapping) & missing(columns)) {
#   columns <- mapping
#   mapping <- NULL
# }
# GGally:::stop_if_bad_mapping(mapping)
# columns <- GGally:::fix_column_values(
#   data, columns, columnLabels, "columns", "columnLabels"
# )
# GGally:::stop_if_high_cardinality(data, columns, cardinality_threshold)
upper <- GGally:::check_and_set_ggpairs_defaults(
  "upper", upper,
  continuous = "cor", combo = "box_no_facet", discrete = "count",
  na = "na"
)
lower <- GGally:::check_and_set_ggpairs_defaults(
  "lower", lower,
  continuous = "points", combo = "facethist", discrete = "facetbar",
  na = "na"
)
diag <- GGally:::check_and_set_ggpairs_defaults(
  "diag", diag,
  continuous = "densityDiag", discrete = "barDiag",
  na = "naDiag", isDiag = TRUE
)
axisLabels <- GGally:::fix_axis_label_choice(
  axisLabels, c("show", "internal", "none")
)
# proportions <- GGally:::ggmatrix_proportions(proportions, data, columns)
dataTypes <- GGally:::plot_types(data, columns, columns, allowDiag = TRUE)
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
      ggplot2:::aes_(
        x = as.name(xColName), 
        y = as.name(yColName)
      ),
      mapping
    ),
    types$mapping
  )
  args <- list(
    types = types, sectionAes = sectionAes
  )
  if (plotType == "label") {
    args$label <- columnLabels[posX]
  }
  plot_fn <- GGally:::ggmatrix_plot_list(plotType)
  p <- do.call(plot_fn, args)
  return(p)
})
plotMatrix <- GGally::ggmatrix(
  plots = ggpairsPlots, byrow = TRUE, 
  nrow = length(columns), ncol = length(columns),
  xAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
  yAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
  labeller = labeller, switch = switch, 
  showStrips = showStrips,
  showXAxisPlotLabels = identical(axisLabels, "show"),
  showYAxisPlotLabels = identical(axisLabels, "show"),
  title = title, xlab = xlab, ylab = ylab, 
  data = data_, gg = NULL, progress = progress, legend = legend, 
  xProportions = proportions, yProportions = proportions
)
plotMatrix
