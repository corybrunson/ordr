
# experiments

# ordination
( iris_pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE) )

# TODO: Group these by plot rather than by syntax?

# TODO: Assemble representative errors to ensure meaningful error messages.

# current `stat_(rows|cols)_*()`

# trivial stat
ggbiplot(iris_pca) +
  stat_cols(geom = "vector", aes(label = name)) +
  stat_rows(geom = GeomPoint, aes(color = Species), alpha = .5)
# non-referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_cols(geom = "axis", aes(label = name, center = center, scale = scale)) +
  stat_rows_spantree(aes(color = Species))
# referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_cols_rule(aes(label = name, center = center, scale = scale)) +
  stat_rows_bagplot(aes(color = Species, fill = Species))
# subsets & elements
ordinate(iris, princomp, cols = 1:4, cor = TRUE) |> 
  ggbiplot(axis.type = "predictive") +
  stat_cols_rule(
    aes(label = name, center = center, scale = scale),
    ref_elements = "score", subset = 1:2
  ) +
  geom_rows_bagplot(aes(color = Species, fill = Species), elements = "score")

# current `geom_(rows|cols)_*()`

# trivial stat
ggbiplot(iris_pca) +
  geom_cols_vector(aes(label = name)) +
  geom_rows_point(aes(color = Species), alpha = .5)
# non-referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_cols_axis(aes(label = name, center = center, scale = scale)) +
  geom_rows_point(aes(color = Species), alpha = .5)
# referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_cols_rule(
    stat = "rule",
    aes(label = name, center = center, scale = scale)
  ) +
  geom_rows_bagplot(aes(color = Species, fill = Species))
# subsets & elements
ordinate(iris, princomp, cols = 1:4, cor = TRUE) |> 
  ggbiplot(axis.type = "predictive") +
  geom_cols_rule(
    stat = "rule",
    aes(label = name, center = center, scale = scale),
    ref_elements = "score", subset = 1:2
  ) +
  geom_rows_bagplot(aes(color = Species, fill = Species), elements = "score")

# experimental `geom_*()` with `(rows|cols)_stat()`
# TODO: Set default `matrix_stat(stat = StatIdentity)`.

# trivial stat
ggbiplot(iris_pca) +
  geom_vector(stat = cols_stat("identity"), aes(label = name)) +
  geom_point(stat = rows_stat(StatIdentity), aes(color = Species), alpha = .5)
# non-referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_axis(
    stat = cols_stat("identity"),
    aes(label = name, center = center, scale = scale)
  ) +
  geom_point(
    stat = rows_stat(StatIdentity),
    aes(color = Species), alpha = .5
  )
# referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  geom_rule(
    stat = cols_stat("rule"),
    aes(label = name, center = center, scale = scale)
  ) +
  geom_bagplot(
    stat = rows_stat(StatBagplot),
    aes(color = Species, fill = Species)
  )
# subsets & elements
ordinate(iris, princomp, cols = 1:4, cor = TRUE) |> 
  ggbiplot(axis.type = "predictive") +
  geom_rule(
    stat = cols_stat(StatRule),
    aes(label = name, center = center, scale = scale),
    ref_elements = "score", subset = 1:2
  ) +
  geom_bagplot(
    stat = rows_stat("bagplot"),
    aes(color = Species, fill = Species),
    elements = "score"
  )

# experimental `stat_*()` with `data = (rows|cols)_data()`

# trivial stat
ggbiplot(iris_pca) +
  stat_identity(geom = GeomVector, data = cols_data(), aes(label = name)) +
  stat_identity(
    geom = "point", data = rows_data(), aes(color = Species),
    alpha = .5
  )
# non-referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_identity(
    geom = GeomAxis, data = cols_data(),
    aes(label = name, center = center, scale = scale)
  ) +
  stat_spantree(data = rows_data(), aes(color = Species))
# referential stat
# NOTE: Requires development version of {gggda} @932e5f8.
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_rule(
    data = cols_data(),
    referent = rows_data(),
    aes(label = name, center = center, scale = scale)
  ) +
  stat_bagplot(data = rows_data(), aes(color = Species, fill = Species))
# subsets & elements
ordinate(iris, princomp, cols = 1:4, cor = TRUE) |> 
  ggbiplot(axis.type = "predictive") +
  stat_rule(
    data = cols_data(subset = 1:2),
    referent = rows_data(elements = "score"),
    aes(label = name, center = center, scale = scale)
  ) +
  stat_bagplot(
    data = rows_data(elements = "score"),
    aes(color = Species, fill = Species)
  )

# current `layer()` with `(rows|cols)_stat()`

# trivial stat
ggbiplot(iris_pca) +
  layer(
    stat = cols_stat("identity"), geom = "vector", position = "identity",
    mapping = aes(label = name, center = center, scale = scale)
  ) +
  layer(
    stat = rows_stat(StatIdentity), geom = GeomPoint, position = "identity",
    mapping = aes(color = Species), params = list(alpha = .5)
  )
# non-referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  layer(
    stat = cols_stat("identity"), geom = "axis", position = "identity",
    mapping = aes(label = name, center = center, scale = scale)
  ) +
  layer(
    stat = StatSpantree, geom = "segment", position = PositionIdentity,
    mapping = aes(color = Species)
  )
# referential stat
ggbiplot(iris_pca, axis.type = "predictive") +
  layer(
    stat = cols_stat("rule"), geom = GeomRule, position = PositionIdentity,
    mapping = aes(label = name, center = center, scale = scale)
  ) +
  layer(
    stat = rows_stat(StatBagplot), geom = "bagplot", position = "identity",
    mapping = aes(color = Species, fill = Species)
  )
# subsets & elements
ordinate(iris, princomp, cols = 1:4, cor = TRUE) |> 
  ggbiplot(axis.type = "predictive") +
  layer(
    stat = cols_stat(StatRule), geom = GeomRule, position = PositionIdentity,
    mapping = aes(label = name, center = center, scale = scale),
    params = list(ref_elements = "score", subset = 1:2)
  ) +
  layer(
    stat = rows_stat("bagplot"), geom = "bagplot", position = "identity",
    mapping = aes(color = Species, fill = Species),
    params = list(elements = "score")
  )

# TODO: Decide whether to implement below; maybe only above for next release.

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

# Gina's idea
make_elts_stat_ggproto(StatCenter, "rows") |> 
  make_constructor(geom = "point") ->
  stat_rows_center2
StatCenter |> 
  make_constructor(geom = "point", data = rows_data()) ->
  stat_rows_center3
ggbiplot(iris_pca, axis.type = "predictive") +
  stat_rows_center3(aes(color = Species), size = 5) +
  geom_rows_point(aes(color = Species), alpha = .5)
# options for layers not pre-built:
# 1. `data = (rows|cols)_data()`
# 2. stat = `(rows|cols)_stat(<stat>)`
# 3. `make_constructor()` with one of the above
