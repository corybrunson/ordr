#' @title Vectors from the origin
#' 
#' @description `geom_vector()` renders arrows from the origin to points.

#' @details Vectors are positions relative to some common reference point, in
#'   this case the origin; they comprise direction and magnitude. Vectors are
#'   usually represented with arrows rather than markers (points). They are
#'   commonly used to represent variables in biplots, as by Greenacre (2010).
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
#' - `size`
#' - `head`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param arrow Specification for arrows, as created by [grid::arrow()], or else
#'   `NULL` for no arrows.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-vector.r
#' @export
geom_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  arrow = default_arrow,
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
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomVector <- ggproto(
  "GeomVector", GeomSegment,
  
  required_aes = c("x", "y"),
  non_missing_aes = c("x", "y"),
  
  default_aes = aes(linewidth = 0.5, linetype = 1, colour = "black", alpha = NA,
                    head = "closed"),
  
  setup_data = function(data, params) {
    
    # all vectors have tails at the origin
    data <- transform(
      data,
      xend = 0, yend = 0
    )
    
    # pre-process `head` aesthetic
    if (is.null(data[["head"]])) return(data)
    if (is.character(data$head) || is.factor(data$head)) {
      data$head <- match.arg(data$head, c("open", "closed"), several.ok = TRUE)
    } else if (is.numeric(data$head) || is.logical(data$head)) {
      data$head <- c("open", "closed")[as.integer(data$head) + 1L]
    }
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    arrow = default_arrow,
    lineend = "round", linejoin = "mitre",
    na.rm = FALSE
  ) {
    
    data <- ensure_cartesian_polar(data)
    
    if (! coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    # initialize grob list
    grobs <- list()
    
    # separate grobs for open and closed arrows
    for (type in unique(data$head)) {
      arrow$type <- match(type, c("open", "closed"))
      if (is.na(type)) arrow <- NULL
      grobs <- c(grobs, list(GeomSegment$draw_panel(
        data = subset(data, head == type | (is.na(head) & is.na(type))),
        panel_params = panel_params, coord = coord,
        arrow = arrow, lineend = lineend, linejoin = linejoin,
        na.rm = na.rm
      )))
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_vector")
    grob
  }
)
