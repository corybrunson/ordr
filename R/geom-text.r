#' @title Render text and labels at ordinates
#' 

#' @description `geom_*_text()` renders text, and `geom_*_label()` labels, at
#'   the positions of the subjects or vectors. `geom_*_text_repel()` and
#'   `geom_*_label_repel()` invoke functionality from the
#'   **[ggrepel][ggrepel::ggrepel]** package.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_text()`, `geom_*_label()`, `geom_*_text_repel()`, and
#' `geom_*_label_repel()` understand the following aesthetics (required
#' aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - **`label`**
#' - `alpha`
#' - `angle`
#' - `colour`
#' - `family`
#' - `fontface`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#' - `group`
#' 

#' @name geom-biplot-text
#' @import ggplot2
#' @importFrom ggrepel GeomTextRepel GeomLabelRepel
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param parse,check_overlap,nudge_x,nudge_y See [ggplot2::geom_text()].
#' @template param-matrix
#' @example inst/examples/finches-ca.r
#' @example inst/examples/country-prcomp-confer.r
#' @example inst/examples/benthos-ca-augment-confer.r
NULL

#' @rdname geom-biplot-text
#' @export
geom_rows_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || ! missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_cols_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || ! missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_biplot_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || ! missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_rows_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_cols_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_biplot_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @export
ggrepel::geom_text_repel
#' @export
ggrepel::geom_label_repel

#' @rdname geom-biplot-text
#' @export
geom_rows_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_cols_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_biplot_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_rows_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_cols_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-text
#' @export
geom_biplot_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}
