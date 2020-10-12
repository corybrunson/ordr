#' @title Render text at ordinates radiating out from the origin
#' 

#' @description `geom_*_text_radiate()` is adapted from [ggbiplot::ggbiplot()].
#'   It renders text at specified positions and angles that radiate out from the
#'   origin.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_text_radiate()` understands the following aesthetics (required
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

#' @name geom-biplot-text-radiate
#' @import ggplot2
#' @inheritParams geom-biplot-text
#' @example inst/examples/bioenv-mlm.r
NULL

#' @rdname geom-biplot-text-radiate
#' @usage NULL
#' @export
GeomTextRadiate <- ggproto(
  "GeomTextRadiate", GeomText,
  
  draw_panel = function(
    data, panel_params, coord,
    parse = FALSE,
    na.rm = FALSE,
    check_overlap = FALSE
  ) {
    
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    data$hjust <- 0.5 + (data$hjust - 0.625 - 0.5) * sign(data$x)
    data$angle <- as.numeric(data$angle) + (180 / pi) * atan(data$y / data$x)
    
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }
    
    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    
    grid::textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = grid::gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  }
)

#' @rdname geom-biplot-text-radiate
#' @export
geom_text_radiate <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextRadiate,
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

#' @rdname geom-biplot-text-radiate
#' @export
geom_u_text_radiate <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomTextRadiate,
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

#' @rdname geom-biplot-text-radiate
#' @export
geom_v_text_radiate <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomTextRadiate,
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

#' @rdname geom-biplot-text-radiate
#' @export
geom_biplot_text_radiate <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomTextRadiate,
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

# not exported from *ggplot2*
#' @importFrom utils getFromNamespace
compute_just <- getFromNamespace("compute_just", "ggplot2")
