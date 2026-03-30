
# helper functions for alternative layer syntaces

# TODO: These might be introduced before the transition to lazy ggproto/shortcut
# construction.

# for
# * `stat_*(data = rows_data(subset = , elements = ), ...)`
# * `stat_*(data = cols_data(subset = , elements = ), ...)`
# TODO: Obviate re-assignment of `.matrix`.
rows_data <- function(subset = NULL, elements = "active") {
  function(data) {
    res <- setup_rows_data(data, list(subset = subset, elements = elements))
    res$.matrix <- "rows"
    res
  }
}
cols_data <- function(subset = NULL, elements = "active") {
  function(data) {
    res <- setup_cols_data(data, list(subset = subset, elements = elements))
    res$.matrix <- "cols"
    res
  }
}

# for
# * `geom_*(stat = <rows|cols>_stat(Stat*, subset = , elements = )`
# * `geom_*(stat = <rows|cols>_stat("*", subset = , elements = )`

# TODO: Return a ggproto rather than a character string?
# TODO: Redefine `ordr::rows_stat()` and `ordr::cols_stat()` to match these.
rows_stat <- function(stat, subset = NULL, elements = "active") {
  if (inherits(stat, "ggproto")) 
    stat <- gsub("^stat\\_", "", ggplot2:::snake_class(stat))
  matrix_stat("rows", stat)
}
cols_stat <- function(stat, subset = NULL, elements = "active") {
  if (inherits(stat, "ggproto")) 
    stat <- gsub("^stat\\_", "", ggplot2:::snake_class(stat))
  matrix_stat("cols", stat)
}

# ggproto adapters

make_elts_stat_ggproto <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  .Matrix <- switch(.matrix, rows = "Rows", cols = "Cols")
  if (is.character(x)) x <- ggplot2:::validate_subclass(x, "Stat")
  stat_constructor <- ggplot2:::snake_class(x)
  setup_data_fn <-
    switch(.matrix, rows = setup_rows_xy_data, cols = setup_cols_xy_data)
  StatEltsTransform <- ggproto(
    gsub("^Stat", paste0("Stat", .Matrix), class(x)[[1L]]), x,
    setup_data = setup_data_fn,
    compute_group = ord_formals(x, "compute_group")
  )
  if (inherits(get(stat_constructor)(), "LayerRef")) {
    StatEltsTransform$extra_params <-
      c(x$extra_params, "ref_subset", "ref_elements")
    StatEltsTransform$setup_params <- setup_referent_params
  }
  StatEltsTransform
}

# illustration of basic `make_constructor()`
make_constructor(make_elts_stat_ggproto(StatChull, "rows"), geom = "polygon")

# needs to accept any stat & convert it within `layer()`
make_rows_constructor <- function(x, ...) {
  UseMethod("make_rows_constructor")
}
make_rows_constructor.Geom <- function(
    x,
    ...,
    checks = rlang::exprs(),
    omit = character(),
    env = rlang::caller_env()
) {
  dots <- rlang::list2(...)
  dots$stat <- if ("stat" %in% names(dots)) rows_stat(dots$stat) else "rows"
  do.call(
    make_constructor,
    args = c(list(x = x), dots, list(checks = checks, omit = omit, env = env))
  )
}
# FIXME: Need to adapt `make_constructor.Geom()` source code from original.
# make_rows_constructor.Geom <- ...
# geom_rows_point2 <- make_rows_constructor(GeomPoint)

# draft implementation to allow user to specify basic stat and geom
# and have the stat appropriately biplot-specified
layer_rows <- function(
    geom = NULL,
    stat = NULL,
    data = NULL,
    mapping = NULL, 
    position = NULL,
    params = list(),
    inherit.aes = TRUE,
    check.aes = TRUE, 
    check.param = TRUE,
    show.legend = NA,
    key_glyph = NULL,
    layout = NULL,
    layer_class = ggplot2:::Layer
) {
  # create something like `StatRowsTransform`
  StatProto <- make_elts_stat_ggproto(stat, "rows")
  # determine whether to use `LayerRef`
  is_ref_layer <- "ref_subset" %in% StatProto$extra_params
  # create layer
  LayerResult <- layer(
    geom = geom,
    stat = StatProto,
    data = data,
    mapping = mapping, 
    position = position,
    params = params,
    inherit.aes = inherit.aes,
    check.aes = check.aes, 
    check.param = check.param,
    show.legend = show.legend,
    key_glyph = key_glyph,
    layout = layout, 
    layer_class = layer_class
  )
  if (is_ref_layer) class(LayerResult) <- c("LayerRef", class(LayerResult))
  LayerResult
}
layer_cols <- function(
    geom = NULL,
    stat = NULL,
    data = NULL,
    mapping = NULL, 
    position = NULL,
    params = list(),
    inherit.aes = TRUE,
    check.aes = TRUE, 
    check.param = TRUE,
    show.legend = NA,
    key_glyph = NULL,
    layout = NULL,
    layer_class = ggplot2:::Layer
) {
  # create something like `StatColsTransform`
  StatProto <- make_elts_stat_ggproto(stat, "cols")
  # determine whether to use `LayerRef`
  is_ref_layer <- "ref_subset" %in% StatProto$extra_params
  # create layer
  LayerResult <- layer(
    geom = geom,
    stat = StatProto,
    data = data,
    mapping = mapping, 
    position = position,
    params = params,
    inherit.aes = inherit.aes,
    check.aes = check.aes, 
    check.param = check.param,
    show.legend = show.legend,
    key_glyph = key_glyph,
    layout = layout, 
    layer_class = layer_class
  )
  if (is_ref_layer) class(LayerResult) <- c("LayerRef", class(LayerResult))
  LayerResult
}

# experiments

# ordination
iris_pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)
# FIXME
# current `stat_(rows|cols)()`
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_cols(geom = "axis", aes(label = name, center = center, scale = scale)) +
  stat_rows(geom = GeomPoint, aes(color = Species), alpha = .5)
# current `(stat|geom)_(rows|cols)()`
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  geom_rows_point(aes(color = Species), alpha = .5)
# experimental `stat_*()` with `data = (rows|cols)_data()`
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_identity(
    geom = GeomAxis, data = cols_data(),
    aes(label = name, center = center, scale = scale)
  ) +
  stat_identity(
    geom = "point", data = rows_data(),
    aes(color = Species), alpha = .5
  )
# experimental `geom_*()` with `(rows|cols)_stat()`
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_axis(
    stat = cols_stat("identity"),
    aes(label = name, center = center, scale = scale)
  ) +
  geom_point(
    stat = rows_stat(StatIdentity),
    aes(color = Species), alpha = .5
  )
# current `layer()`
ggbiplot(iris_pca, axis.type = "predictive") +
  layer(
    geom = GeomAxis, stat = cols_stat("identity"), position = "identity",
    mapping = aes(label = name, center = center, scale = scale)
  ) +
  layer(
    geom = "point", stat = rows_stat(StatIdentity), position = PositionIdentity,
    mapping = aes(color = Species), params = list(alpha = .5)
  )
# experimental `layer_(rows|cols)()`
ggbiplot(iris_pca, axis.type = "predictive") +
  layer_cols(
    geom = GeomAxis, stat = "identity", position = "identity",
    mapping = aes(label = name, center = center, scale = scale)
  ) +
  layer_rows(
    geom = "point", stat = StatIdentity, position = "identity",
    mapping = aes(color = Species), params = list(alpha = .5)
  )
