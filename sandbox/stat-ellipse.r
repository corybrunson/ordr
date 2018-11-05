#' Compute normal confidence ellipses for ordination factors
#' 
#' These ordination stats are adapted from \code{\link[ggplot2]{stat_ellipse}}.
#' 

#' @template ggbiplot-layers

#' @name ggbiplot-ellipse
#' @inheritParams ggplot2::layer
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.

#' @rdname ggbiplot-ellipse
#' @usage NULL
#' @export
StatUEllipse <- ggproto(
  "StatUEllipse", "StatEllipse",
  
  compute_group = function(data, scales, type = "t", level = 0.95,
                           segments = 51, na.rm = FALSE) {
    data <- data[data$.matrix == "u", , drop = FALSE]
    
    calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                      level = level, segments = segments)
  }
)

#' @rdname ggbiplot-ellipse
#' @usage NULL
#' @export
StatVEllipse <- ggproto(
  "StatVEllipse", "StatEllipse",
  
  compute_group = function(data, scales, type = "t", level = 0.95,
                           segments = 51, na.rm = FALSE) {
    data <- data[data$.matrix == "v", , drop = FALSE]
    
    calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                      level = level, segments = segments)
  }
)

#' @rdname ggbiplot-ellipse
#' @export
stat_u_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}

#' @rdname ggbiplot-ellipse
#' @export
stat_v_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}

calculate_ellipse <- ggplot2:::calculate_ellipse
