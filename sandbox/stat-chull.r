#' Restrict ordination data to the convex hulls of both matrix factors
#' 
#' 

#' @template ggbiplot-layers

#' @name ggbiplot-chull
#' @inheritParams ggplot2::layer
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.

#' @rdname ggbiplot-chull
#' @usage NULL
#' @export
StatChull <- ggproto(
  "StatChull", Stat,
  
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  },
  
  required_aes = c("x", "y")
  
)

#' @rdname ggbiplot-chull
#' @export
stat_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-chull
#' @usage NULL
#' @export
StatBiplotChull <- ggproto(
  "StatBiplotChull", StatChull,
  
  compute_group = function(data, scales) {
    
    data_u <- data[data$.matrix == "u", , drop = FALSE]
    data_u <- data_u[chull(data_u$x, data_u$y), , drop = FALSE]
    
    data_v <- data[data$.matrix == "v", , drop = FALSE]
    data_v <- data_v[chull(data_v$x, data_v$y), , drop = FALSE]
    
    rbind(data_u, data_v)
  }
  
)

#' @rdname ggbiplot-chull
#' @export
stat_biplot_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
