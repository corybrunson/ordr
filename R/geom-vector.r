#' Render axes vectors, with or without angled labels
#' 

#' \code{geom_*_vector} renders arrows from the origin to the position of each
#' subject or vector. \code{geom_*_axis} is adapted from
#' \strong{link[ggbiplot]{ggbiplot}}. In addition to the arrows, it renders
#' labels that expand outward from the arrowheads at the same angles.
#' @template ggbiplot-layers

#' @section Aesthetics:
#' \code{geom_*_vector} and \code{geom_*_axis} understand the following
#' aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}} (\code{geom_*_axis} only)
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{group}
#' }
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer

#' @rdname ggbiplot-vector
#' @usage NULL
#' @export
GeomVector <- ggproto(
  "GeomVector", GeomSegment,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    # all vectors have tails at the origin
    transform(
      data,
      xend = 0, yend = 0
    )
  },
  
  draw_panel = function(
    data, panel_params, coord,
    arrow = default_arrow,
    lineend = "round", linejoin = "mitre",
    na.rm = FALSE
  ) {
    if (!coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    # reverse ends of `arrow`
    arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    ggplot2::GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      na.rm = na.rm
    )
  }
)

#' @rdname ggbiplot-vector
#' @export
geom_u_vector <- function(
  mapping = NULL, data = NULL, position = "identity",
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ggbiplot-vector
#' @export
geom_v_vector <- function(
  mapping = NULL, data = NULL, position = "identity",
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ggbiplot-vector
#' @export
geom_biplot_vector <- function(
  mapping = NULL, data = NULL, position = "identity",
  .matrix = "v", arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = .matrix,
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

# https://github.com/vqv/ggbiplot/tree/experimental

#' @rdname ggbiplot-vector
#' @usage NULL
#' @export
GeomAxis <- ggproto(
  "GeomAxis", GeomSegment,
  
  required_aes = c("x", "y", "label"),
  
  default_aes = aes(
    size = 0.5,
    linetype = 1,
    colour = "black",
    alpha = NA,
    textsize = 3,
    family = "",
    fontface = 1,
    lineheight = 1.2
  ),
  
  setup_data = GeomVector$setup_data,
  
  draw_panel = function(
    data, panel_params, coord,
    arrow = default_arrow,
    lineend = "round", linejoin = "mitre",
    parse = FALSE,
    na.rm = FALSE
  ) {
    if (is_empty(data)) return(zeroGrob())
    
    text <- transform(
      data, 
      size = textsize,
      angle = (180 / pi) * atan(y / x),
      hjust = 0.5 * (1 - 1.25 * sign(x)),
      vjust = 0.5
    )
    vec <- transform(
      data, 
      textsize = NULL, family = NULL, fontface = NULL, lineheight = NULL
    )
    
    grid::gList(
      GeomText$draw_panel(
        text, panel_params, coord,
        parse = parse,
        na.rm = na.rm
      ),
      GeomVector$draw_panel(
        vec, panel_params, coord,
        arrow = default_arrow,
        lineend = "round", linejoin = "mitre",
        na.rm = na.rm
      )
    )
  }
)

#' @rdname ggbiplot-vector
#' @export
geom_u_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  arrow = default_arrow, 
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ggbiplot-vector
#' @export
geom_v_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  arrow = default_arrow, 
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ggbiplot-vector
#' @export
geom_biplot_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  .matrix = "v", arrow = default_arrow, 
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = .matrix,
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

default_arrow <- grid::arrow(
  angle = 20,
  length = unit(.03, "native"),
  ends = "last",
  type = "closed"
)
