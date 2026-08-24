# Convenience geoms for row and column matrix factors

These geometric element layers (geoms) pair conventional **ggplot2**
geoms with [`stat_rows()`](stat_rows.md) or
[`stat_cols()`](stat_rows.md) in order to render elements for one or the
other matrix factor of a tbl_ord. They understand the same aesthetics as
their corresponding conventional geoms.

## Usage

``` r
geom_rows_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_path(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_path(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_polygon(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  rule = "evenodd",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_polygon(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  rule = "evenodd",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_contour(
  mapping = NULL,
  data = NULL,
  stat = "contour",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_contour(
  mapping = NULL,
  data = NULL,
  stat = "contour",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_density_2d(
  mapping = NULL,
  data = NULL,
  stat = "density_2d",
  position = "identity",
  ...,
  contour_var = "density",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_density_2d(
  mapping = NULL,
  data = NULL,
  stat = "density_2d",
  position = "identity",
  ...,
  contour_var = "density",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_density_2d_filled(
  mapping = NULL,
  data = NULL,
  stat = "density_2d_filled",
  position = "identity",
  ...,
  contour_var = "density",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_density_2d_filled(
  mapping = NULL,
  data = NULL,
  stat = "density_2d_filled",
  position = "identity",
  ...,
  contour_var = "density",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_text(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_text(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = 0.25,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = 0.25,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_text_repel(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  point.padding = 1e-06,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = FALSE,
  inherit.aes = TRUE
)

geom_cols_text_repel(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  point.padding = 1e-06,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = FALSE,
  inherit.aes = TRUE
)

geom_rows_label_repel(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  label.padding = 0.25,
  point.padding = 1e-06,
  label.r = 0.15,
  label.size = 0.25,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = FALSE,
  inherit.aes = TRUE
)

geom_cols_label_repel(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  label.padding = 0.25,
  point.padding = 1e-06,
  label.r = 0.15,
  label.size = 0.25,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = FALSE,
  inherit.aes = TRUE
)

geom_rows_axis(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  by = NULL,
  num = NULL,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  ...,
  axis.colour = NULL,
  axis.color = NULL,
  axis.alpha = NULL,
  label.angle = 0,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  tick.linewidth = 0.25,
  tick.colour = NULL,
  tick.color = NULL,
  tick.alpha = NULL,
  text.size = 2.6,
  text.angle = 0,
  text.hjust = 0.5,
  text.vjust = 0.5,
  text.family = NULL,
  text.fontface = NULL,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_axis(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  by = NULL,
  num = NULL,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  ...,
  axis.colour = NULL,
  axis.color = NULL,
  axis.alpha = NULL,
  label.angle = 0,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  tick.linewidth = 0.25,
  tick.colour = NULL,
  tick.color = NULL,
  tick.alpha = NULL,
  text.size = 2.6,
  text.angle = 0,
  text.hjust = 0.5,
  text.vjust = 0.5,
  text.family = NULL,
  text.fontface = NULL,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_pointranges(
  mapping = NULL,
  data = NULL,
  stat = "center",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_pointranges(
  mapping = NULL,
  data = NULL,
  stat = "center",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_lineranges(
  mapping = NULL,
  data = NULL,
  stat = "center",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_lineranges(
  mapping = NULL,
  data = NULL,
  stat = "center",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_isoline(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  isoline_text = TRUE,
  by = NULL,
  num = NULL,
  text_dodge = 0.03,
  ...,
  text.size = 3,
  text.angle = 0,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_isoline(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  isoline_text = TRUE,
  by = NULL,
  num = NULL,
  text_dodge = 0.03,
  ...,
  text.size = 3,
  text.angle = 0,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_text_radiate(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_text_radiate(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_vector(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  arrow = default_arrow,
  lineend = "round",
  linejoin = "mitre",
  vector_labels = TRUE,
  ...,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_vector(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  arrow = default_arrow,
  lineend = "round",
  linejoin = "mitre",
  vector_labels = TRUE,
  ...,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_bagplot(
  mapping = NULL,
  data = NULL,
  stat = "bagplot",
  position = "identity",
  ...,
  bag.linewidth = sync(),
  bag.linetype = sync(),
  bag.colour = "black",
  bag.color = NULL,
  bag.fill = sync(),
  bag.alpha = NA,
  median.shape = 21L,
  median.stroke = sync(),
  median.size = 5,
  median.colour = sync(),
  median.color = NULL,
  median.fill = "white",
  median.alpha = NA,
  fence.linewidth = 0.25,
  fence.linetype = 0L,
  fence.colour = sync(),
  fence.color = NULL,
  fence.fill = sync(),
  fence.alpha = 0.25,
  outlier.shape = sync(),
  outlier.stroke = sync(),
  outlier.size = sync(),
  outlier.colour = sync(),
  outlier.color = NULL,
  outlier.fill = NA,
  outlier.alpha = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_bagplot(
  mapping = NULL,
  data = NULL,
  stat = "bagplot",
  position = "identity",
  ...,
  bag.linewidth = sync(),
  bag.linetype = sync(),
  bag.colour = "black",
  bag.color = NULL,
  bag.fill = sync(),
  bag.alpha = NA,
  median.shape = 21L,
  median.stroke = sync(),
  median.size = 5,
  median.colour = sync(),
  median.color = NULL,
  median.fill = "white",
  median.alpha = NA,
  fence.linewidth = 0.25,
  fence.linetype = 0L,
  fence.colour = sync(),
  fence.color = NULL,
  fence.fill = sync(),
  fence.alpha = 0.25,
  outlier.shape = sync(),
  outlier.stroke = sync(),
  outlier.size = sync(),
  outlier.colour = sync(),
  outlier.color = NULL,
  outlier.fill = NA,
  outlier.alpha = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_rule(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  by = NULL,
  num = NULL,
  snap_rule = TRUE,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  ...,
  axis.colour = NULL,
  axis.color = NULL,
  axis.alpha = NULL,
  label.angle = 0,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  tick.linewidth = 0.25,
  tick.colour = NULL,
  tick.color = NULL,
  tick.alpha = NULL,
  text.size = 2.6,
  text.angle = 0,
  text.hjust = 0.5,
  text.vjust = 0.5,
  text.family = NULL,
  text.fontface = NULL,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_rule(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  by = NULL,
  num = NULL,
  snap_rule = TRUE,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  ...,
  axis.colour = NULL,
  axis.color = NULL,
  axis.alpha = NULL,
  label.angle = 0,
  label.colour = NULL,
  label.color = NULL,
  label.alpha = NULL,
  tick.linewidth = 0.25,
  tick.colour = NULL,
  tick.color = NULL,
  tick.alpha = NULL,
  text.size = 2.6,
  text.angle = 0,
  text.hjust = 0.5,
  text.vjust = 0.5,
  text.family = NULL,
  text.fontface = NULL,
  text.colour = NULL,
  text.color = NULL,
  text.alpha = NULL,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rows_interpolation(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  new_data = NULL,
  type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  point.fill = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cols_interpolation(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  new_data = NULL,
  type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  point.fill = NA,
  na.rm = FALSE,
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

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
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

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

- arrow:

  Arrow specification, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- rule:

  Either `"evenodd"` or `"winding"`. If polygons with holes are being
  drawn (using the `subgroup` aesthetic) this argument defines how the
  hole coordinates are interpreted. See the examples in
  [`grid::pathGrob()`](https://rdrr.io/r/grid/grid.path.html) for an
  explanation.

- bins:

  Number of contour bins. Overridden by `breaks`.

- binwidth:

  The width of the contour bins. Overridden by `bins`.

- breaks:

  One of:

  - Numeric vector to set the contour breaks

  - A function that takes the range of the data and binwidth as input
    and returns breaks as output. A function can be created from a
    formula (e.g. ~ fullseq(.x, .y)).

  Overrides `binwidth` and `bins`. By default, this is a vector of
  length ten with [`pretty()`](https://rdrr.io/r/base/pretty.html)
  breaks.

- contour_var:

  Character string identifying the variable to contour by. Can be one of
  `"density"`, `"ndensity"`, or `"count"`. See the section on computed
  variables for details.

- parse:

  If `TRUE`, the labels will be parsed into expressions and displayed as
  described in [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html).

- nudge_x, nudge_y:

  Horizontal and vertical adjustments to nudge the starting position of
  each text label. The units for `nudge_x` and `nudge_y` are the same as
  for the data units on the x-axis and y-axis.

- check_overlap:

  If `TRUE`, text that overlaps previous text in the same layer will not
  be plotted. `check_overlap` happens at draw time and in the order of
  the data. Therefore data should be arranged by the label column before
  calling
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
  Note that this argument is not supported by
  [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- size.unit:

  How the `size` aesthetic is interpreted: as millimetres (`"mm"`,
  default), points (`"pt"`), centimetres (`"cm"`), inches (`"in"`), or
  picas (`"pc"`).

- label.padding:

  Amount of padding around label. Defaults to 0.25 lines.

- label.r:

  Radius of rounded corners. Defaults to 0.15 lines.

- label.size:

  **\[deprecated\]** Replaced by the `linewidth` aesthetic. Size of
  label border, in mm.

- box.padding:

  Amount of padding around bounding box, as unit or number. Defaults to
  0.25. (Default unit is lines, but other units can be specified by
  passing `unit(x, "units")`).

- point.padding:

  Amount of padding around labeled point, as unit or number. Defaults
  to 0. (Default unit is lines, but other units can be specified by
  passing `unit(x, "units")`).

- min.segment.length:

  Skip drawing segments shorter than this, as unit or number. Defaults
  to 0.5. (Default unit is lines, but other units can be specified by
  passing `unit(x, "units")`).

- force:

  Force of repulsion between overlapping text labels. Defaults to 1.

- force_pull:

  Force of attraction between a text label and its corresponding data
  point. Defaults to 1.

- max.time:

  Maximum number of seconds to try to resolve overlaps. Defaults to 0.5.

- max.iter:

  Maximum number of iterations to try to resolve overlaps. Defaults to
  10000.

- max.overlaps:

  Exclude text labels when they overlap too many other things. For each
  text label, we count how many other text labels or other data points
  it overlaps, and exclude the text label if it has too many overlaps.
  Defaults to 10.

- xlim, ylim:

  Limits for the x and y axes. Text labels will be constrained to these
  limits. By default, text labels are constrained to the entire plot
  area.

- direction:

  direction of stairs: 'vh' for vertical then horizontal, 'hv' for
  horizontal then vertical, or 'mid' for step half-way between adjacent
  x-values.

- seed:

  Random seed passed to
  [`set.seed`](https://rdrr.io/r/base/Random.html). Defaults to `NA`,
  which means that `set.seed` will not be called.

- verbose:

  If `TRUE`, some diagnostics of the repel algorithm are printed

- axis_labels, axis_ticks, axis_text:

  Logical; whether to include labels, tick marks, and text value marks
  along the axes.

- by, num:

  Intervals between elements or number of elements; specify only one.

- tick_length:

  Numeric; the length of the tick marks, as a proportion of the minimum
  of the plot width and height.

- text_dodge:

  Numeric; the orthogonal distance of tick mark text from the axis, as a
  proportion of the minimum of the plot width and height.

- label_dodge:

  Numeric; the orthogonal distance of the axis label from the axis, as a
  proportion of the minimum of the plot width and height.

- axis.colour, axis.color, axis.alpha:

  Default aesthetics for axes. Set to NULL to inherit from the data's
  aesthetics.

- label.angle, label.colour, label.color, label.alpha:

  Default aesthetics for labels. Set to NULL to inherit from the data's
  aesthetics.

- tick.linewidth, tick.colour, tick.color, tick.alpha:

  Default aesthetics for tick marks. Set to NULL to inherit from the
  data's aesthetics.

- text.colour, text.color:

  Colour of the text. When `NULL` (default), the `colour` aesthetic
  determines the colour of the text. `text.color` is an alias for
  `text.colour`.

- isoline_text:

  Logical; whether to include text value marks along the isolines.

- vector_labels:

  Logical; whether to include labels radiating outward from the vectors.

- bag.linetype, bag.linewidth, bag.colour, bag.color, bag.fill,
  bag.alpha:

  Default aesthetics for bags. Set to
  [`sync()`](https://corybrunson.github.io/gggda/reference/sync.html) to
  inherit from the data's aesthetics or to `NULL` to use the data's
  aesthetics.

- median.shape, median.stroke, median.size, median.colour, median.color,
  median.fill, median.alpha:

  Default aesthetics for medians. Set to
  [`sync()`](https://corybrunson.github.io/gggda/reference/sync.html) to
  inherit from the data's aesthetics or to `NULL` to use the data's
  aesthetics.

- fence.linetype, fence.linewidth, fence.colour, fence.color,
  fence.fill, fence.alpha:

  Default aesthetics for fences. Set to
  [`sync()`](https://corybrunson.github.io/gggda/reference/sync.html) to
  inherit from the data's aesthetics or to `NULL` to use the data's
  aesthetics.

- outlier.shape, outlier.stroke, outlier.size, outlier.colour,
  outlier.color, outlier.fill, outlier.alpha:

  Default aesthetics for outliers. Set to
  [`sync()`](https://corybrunson.github.io/gggda/reference/sync.html) to
  inherit from the data's aesthetics or to `NULL` to use the data's
  aesthetics.

- snap_rule:

  Logical; whether to snap rule segments to grid values.

- new_data:

  A list (best structured as a
  [data.frame](https://rdrr.io/r/base/data.frame.html)) of row
  (`geom_cols_interpolation()`) or column (`geom_rows_interpolation()`)
  values to interpolate.

- type:

  Character value matched to `"centroid"` or `"sequence"`; the type of
  operations used to visualize interpolation.

- point.fill:

  Default aesthetics for markers. Set to NULL to inherit from the data's
  aesthetics.

## Value

A ggproto [layer](https://ggplot2.tidyverse.org/reference/layer.html).

## See also

Other biplot layers: [`biplot-stats`](biplot-stats.md),
[`stat_rows()`](stat_rows.md)

## Examples

``` r

# compute log-ratio analysis of Freestone primary class composition measurements
glass %>%
  ordinate(cols = c(SiO2, Al2O3, CaO, FeO, MgO),
           model = lra, compositional = TRUE) %>%
  confer_inertia("rows") %>%
  print() -> glass_lra
#> # A tbl_ord of class 'lra': (68 × 4) · (5 × 4)´
#> # 4 coordinates: LRSV1, LRSV2, ..., LRSV4
#> # Rows (principal, 100%): [ 68 × 4 | 13 ]
#>       LRSV1   LRSV2     LRSV3    LRSV4 | weight .element Site   
#>   [0.01334][0.0034][0.000489][0.00013] | <dbl> <chr>    <chr>  
#>  1  0.0925   0.0929  0.0156    0.00710 | 0.0147 active   Bet El…
#>  2  0.0905   0.0591 -0.0439   -0.00835 | 0.0147 active   Bet El…
#>  3  0.0844   0.0333 -0.000492 -0.00713 | 0.0147 active   Bet El…
#>  4  0.0647   0.0211  0.0267    0.00624 | 0.0147 active   Bet El…
#>  5  0.0635   0.0257  0.0239    0.0159  | 0.0147 active   Bet El…
#>                   ⋮                                 ⋮           
#> # ℹ 10 more variables:
#> #   Anal <chr>,
#> #   Context <chr>,
#> #   Form <chr>,
#> #   TiO2 <dbl>,
#> #   MnO <dbl>,
#> #   Na2O <dbl>, …
#> # Columns (standard, 0%): [ 5 × 4 | 3 ]
#>       LRSV1   LRSV2     LRSV3    LRSV4 | name   weight .element
#>         [1]     [1]       [1]      [1] | <chr>   <dbl> <chr>   
#>  1 -0.00548  0.338   0.237    -0.0538  | SiO2  0.852   active  
#>  2  4.15    -0.714  -2.50      2.65    | Al2O3 0.0313  active  
#>  3 -0.517   -2.95   -0.126    -0.484   | CaO   0.0976  active  
#>  4  0.553    2.23   -9.57     -9.65    | FeO   0.00524 active  
#>  5 -5.61     0.790  -4.47      4.40    | MgO   0.0138  active  
# row-principal biplot with ordinate-wise standard deviations
glass_lra %>%
  ggbiplot(aes(color = Site), sec.axes = "cols") +
  theme_biplot() +
  scale_color_brewer(type = "qual", palette = 6) +
  geom_cols_text(stat = "chull", aes(label = name), color = "#444444") +
  geom_rows_lineranges(fun.data = mean_sdl, linewidth = .75) +
  geom_rows_point(alpha = .5) +
  ggtitle(
    "Row-principal LRA biplot of Freestone glass measurements",
    "Ranges 2 sample standard deviations from centroids"
  )


# principal components analysis of glass composition measurements
glass[, c(5L, 7L, 8L, 10L, 11L)] %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  cbind_rows(site = glass$Site, form = glass$Form) %>%
  augment_ord() %>%
  print() -> glass_pca
#> # A tbl_ord of class 'princomp': (68 × 5) · (5 × 5)´
#> # 5 coordinates: Comp.1, Comp.2, ..., Comp.5
#> # Rows (principal, 100%): [ 68 × 5 | 3 ]
#>    Comp.1   Comp.2  Comp.3  Comp.4  Comp.5 | .element site     form 
#>   [180.9]  [98.44] [47.19] [11.78] [1.733] | <chr>    <chr>    <chr>
#>  1  2.01   0.585    0.940  -0.276  -0.107  | score    Bet Eli… Chunk
#>  2  2.55   0.513   -1.71   -0.193  -0.0602 | score    Bet Eli… Chunk
#>  3  1.64   0.0977   0.131   0.218   0.0182 | score    Bet Eli… Chunk
#>  4  1.07   0.00734  1.20    0.524   0.0958 | score    Bet Eli… Chunk
#>  5  1.07   0.00573  1.31    0.443  -0.0258 | score    Bet Eli… Chunk
#>                     ⋮                                   ⋮           
#> # Columns (standard, 0%): [ 5 × 5 | 4 ]
#>    Comp.1   Comp.2  Comp.3  Comp.4  Comp.5 | name  center scale
#>       [1]      [1]     [1]     [1]     [1] | <chr>  <dbl> <dbl>
#>  1  0.476  0.383    0.388   0.676   0.137  | SiO2  71.7   3.16 
#>  2  0.488 -0.492   -0.0574  0.112  -0.710  | Al2O3  2.64  0.956
#>  3  0.383  0.234   -0.873   0.0620  0.182  | FeO    0.442 0.159
#>  4 -0.425  0.580   -0.153   0.191  -0.651  | MgO    1.15  0.913
#>  5 -0.456 -0.469   -0.247   0.700   0.143  | CaO    8.18  1.36 
#> # ℹ 1 more variable:
#> #   .element <chr>
# note that column standard coordinates are unit vectors
rowSums(get_cols(glass_pca) ^ 2)
#>  SiO2 Al2O3   FeO   MgO   CaO 
#>     1     1     1     1     1 
# plot column standard coordinates with a unit circle underlaid
glass_pca %>%
  ggbiplot(aes(label = name), sec.axes = "cols") +
  theme_biplot() +
  geom_rows_point(aes(color = site, shape = form), elements = "score") +
  geom_unit_circle(alpha = .5, scale.factor = 3) +
  geom_cols_vector()
```
