# Render plot elements for one matrix of an ordination

These stats merely tell
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
which factor of an ordination to pull data from for a plot layer. They
are invoked internally by the various [`geom_*_*()`](biplot-geoms.md)
layers.

## Usage

``` r
stat_rows(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  subset = NULL,
  elements = "active",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_cols(
  mapping = NULL,
  data = NULL,
  geom = "axis",
  position = "identity",
  subset = NULL,
  elements = "active",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- subset:

  An integer, logical, or character vector indicating a subset of rows
  or columns for which to render graphical elements. NB: Internally, the
  `subset` will be taken from the rows of the [fortified](tidiers.md)
  'tbl_ord' comprising rows from only one of the matrix factors. It is
  still possible to pass a formula to the `data` parameter, but it will
  act on the fortified data *before* it has been restricted to one
  matrix factor.

- elements:

  Character vector; which elements of each factor for which to render
  graphical elements. One of `"active"` (the default) or any
  supplementary element type defined by the specific class methods (e.g.
  `"score"` for 'factanal', 'lda_ord', and 'cancord_ord' and
  `"intraset"` and `"interset"` for 'cancor_ord'), via [partial
  matching](https://rdrr.io/r/base/match.arg.html).

- ...:

  Additional arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A ggproto [layer](https://ggplot2.tidyverse.org/reference/layer.html).

## Biplot layers

[`ggbiplot()`](ggbiplot.md) uses
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
internally to produce a single data frame with a `.matrix` column
distinguishing the subjects (`"rows"`) and variables (`"cols"`). The
stat layers `stat_rows()` and `stat_cols()` simply filter the data frame
to one of these two.

The geom layers `geom_rows_*()` and `geom_cols_*()` call the
corresponding stat in order to render plot elements for the
corresponding factor matrix. `geom_dims_*()` selects a default matrix
based on common practice, e.g. points for rows and arrows for columns.

## See also

Other biplot layers: [`biplot-geoms`](biplot-geoms.md),
[`biplot-stats`](biplot-stats.md)

## Examples

``` r
# FA of Swiss social data
swiss_fa <-
  ordinate(swiss, model = factanal, factors = 2L, scores = "regression")
# active and supplementary elements
get_rows(swiss_fa, elements = "active")
#>      Factor1 Factor2
head(get_rows(swiss_fa, elements = "score"))
#>                  Factor1    Factor2
#> Courtelary    0.07912746 -0.6344915
#> Delemont     -0.17926953  1.0783941
#> Franches-Mnt -0.58784929  1.2004233
#> Moutier      -0.42433417 -0.1583409
#> Neuveville    0.38211185 -0.6682790
#> Porrentruy   -0.37286722  1.0884740

# biplot using matrix stats and element filter
ggbiplot(swiss_fa) +
  stat_rows(elements = "score") +
  stat_cols(geom = "vector", aes(label = name))


# biplot using element filter and item selection
# (note that filter precedes selection)
ggbiplot(swiss_fa) +
  geom_rows_point(elements = "score") +
  geom_rows_label(aes(label = name), elements = "score", subset = c(1, 4, 18)) +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  geom_cols_vector(aes(label = name))
#> Warning: Ignoring unknown parameters: `label.size`
```
