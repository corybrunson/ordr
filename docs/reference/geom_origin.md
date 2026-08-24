# Marker or unit circle at the origin

`geom_origin()` renders a symbol, either a set of crosshairs or a
circle, at the origin. `geom_unit_circle()` renders the unit circle,
centered at the origin with radius 1.

## Usage

``` r
geom_origin(
  mapping = NULL,
  data = NULL,
  marker = "crosshairs",
  radius = unit(0.04, "snpc"),
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)

geom_unit_circle(
  mapping = NULL,
  data = NULL,
  segments = 60,
  scale.factor = 1,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
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

- marker:

  The symbol to be drawn at the origin; matched to `"crosshairs"` or
  `"circle"`.

- radius:

  A [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object that sets
  the radius of the crosshairs or of the circle.

- ...:

  Additional arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- na.rm:

  Passed to
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

- segments:

  The number of segments to be used in drawing the circle.

- scale.factor:

  The circle radius; should remain at its default value 1 or passed the
  same value as [`ggbiplot()`](ggbiplot.md). (This is an imperfect fix
  that may be changed in a future version.)

## Value

A ggproto [layer](https://ggplot2.tidyverse.org/reference/layer.html).

## Biplot layers

[`ggbiplot()`](ggbiplot.md) uses
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
internally to produce a single data frame with a `.matrix` column
distinguishing the subjects (`"rows"`) and variables (`"cols"`). The
stat layers [`stat_rows()`](stat_rows.md) and
[`stat_cols()`](stat_rows.md) simply filter the data frame to one of
these two.

The geom layers `geom_rows_*()` and `geom_cols_*()` call the
corresponding stat in order to render plot elements for the
corresponding factor matrix. `geom_dims_*()` selects a default matrix
based on common practice, e.g. points for rows and arrows for columns.

## Aesthetics

`geom_origin()` accepts no aesthetics. `geom_unit_circle()` understands
the following aesthetics (none required):

- `linetype`

- `linewidth`

- `colour`

- `alpha`

## See also

Other geom layers: [`geom_interpolation()`](geom_interpolation.md)

## Examples

``` r
ggplot() +
  theme_void() +
  geom_origin() +
  geom_point(data = seals, aes(delta_long, delta_lat), alpha = .25)

# center each group separately
iris %>%
  split(~ Species) %>%
  lapply(subset, select = -c(Species)) %>%
  lapply(scale, center = TRUE, scale = FALSE) %>%
  lapply(as.data.frame) %>%
  unsplit(iris$Species) %>%
  transform(Species = iris$Species) ->
  iris_ctr
ggplot(iris_ctr, aes(Petal.Width, Petal.Length)) +
  coord_equal() +
  facet_wrap(vars(Species)) +
  geom_unit_circle() +
  geom_point()

# scale group mean differences uniformly
iris_ctr %>%
  subset(select = -c(Species)) %>%
  scale(center = FALSE, scale = TRUE) %>%
  transform(Species = iris$Species) %>%
  ggplot(aes(Petal.Width, Petal.Length)) +
  coord_equal() +
  facet_wrap(vars(Species)) +
  geom_unit_circle() +
  geom_point()

```
