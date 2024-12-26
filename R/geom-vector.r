#' @title Vectors from the origin
#' 
#' @description `geom_vector()` renders arrows from the origin to points,
#'   optionally with text radiating outward.

#' @details Vectors are positions relative to some common reference point, in
#'   this case the origin; they comprise direction and magnitude. Vectors are
#'   usually represented with arrows rather than markers (points). They are
#'   commonly used to represent variables in biplots, as by Greenacre (2010).
#'   This layer, with optional radiating text labels, is adapted from
#'   `ggbiplot()` in the off-CRAN extensions of the same name (Vu, 2014;
#'   Telford, 2017; Gegzna, 2018).
#' 

#' @template ref-greenacre2010

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_vector()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**, **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `label`
#' - `size`
#' - `angle`, `hjust`, `vjust`
#' - `label_colour`, `label_alpha`
#' - `family`, `fontface`, `lineheight`
#' - `group`
#' 

#' @template ref-ggbiplot

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @template param-geom
#' @param arrow Specification for arrows, as created by [grid::arrow()], or else
#'   `NULL` for no arrows.
#' @param vector_labels Logical; whether to include labels radiating outward
#'   from the vectors.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-vector.r
#' @export
geom_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  arrow = default_arrow, lineend = "round", linejoin = "mitre",
  vector_labels = TRUE,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      vector_labels = vector_labels,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomVector <- ggproto(
  "GeomVector", Geom,
  
  required_aes = c("x", "y"),
  non_missing_aes = c("xend", "yend", "linetype", "linewidth", "angle"),
  
  default_aes = aes(
    colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
    label = "", size = 3.88, angle = 0, hjust = .5, vjust = .5,
    label_colour = "black", label_alpha = NA,
    family = "", fontface = 1, lineheight = 1.2
  ),

  setup_data = function(data, params) {
    
    # all vectors have tails at the origin
    transform(
      data,
      xend = 0, yend = 0
    )
  },
  
  draw_panel = function(
    data, panel_params, coord,
    vector_labels = TRUE,
    arrow = default_arrow, lineend = "round", linejoin = "mitre",
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    
    if (! coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    
    # initialize grob list
    grobs <- list()
    
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    grobs <- c(grobs, list(GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      na.rm = na.rm
    )))
    
    if (vector_labels) {
      label_data <- data
      
      # specify aesthetics (if necessary)
      label_data$colour <- label_data$label_colour
      label_data$alpha <- label_data$label_alpha
      label_data$label_colour <- label_data$label_alpha <- NULL
      
      if (is.character(label_data$hjust)) {
        label_data$hjust <- compute_just(label_data$hjust, label_data$x)
      }
      label_data$hjust <- 
        0.5 + (label_data$hjust - 0.625 - 0.5) * sign(label_data$x)
      label_data$angle <- 
        as.numeric(label_data$angle) + 
        (180 / pi) * atan(label_data$y / label_data$x)
      
      lab <- label_data$label
      if (parse) {
        lab <- parse_safe(as.character(lab))
      }
      
      label_data <- coord$transform(label_data, panel_params)
      if (is.character(label_data$vjust)) {
        label_data$vjust <- compute_just(label_data$vjust, label_data$y)
      }
      if (is.character(label_data$hjust)) {
        label_data$hjust <- compute_just(label_data$hjust, label_data$x)
      }
      
      grobs <- c(grobs, list(grid::textGrob(
        lab,
        label_data$x, label_data$y, default.units = "native",
        hjust = label_data$hjust, vjust = label_data$vjust,
        rot = label_data$angle,
        gp = grid::gpar(
          col = alpha(label_data$colour, label_data$alpha),
          fontsize = label_data$size * .pt,
          fontfamily = label_data$family,
          fontface = label_data$fontface,
          lineheight = label_data$lineheight
        ),
        check.overlap = check_overlap
      )))
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_vector")
    grob
  }
)
