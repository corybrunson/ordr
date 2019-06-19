#' @title Restrict ordination data to the convex hulls of both matrix factors
#'
#' @description As used in a **[ggplot2][ggplot2::ggplot2]** vignette, this stat
#'   layer restricts a dataset with `x` and `y` variables to its convex hull.
#'   The biplot extension restricts each matrix factor to its own hull.
#'   

#' @template biplot-layers

#' @name stat-biplot-chull
#' @inheritParams ggplot2::layer
#' @template param-stat
#' @example inst/examples/benthos-ca-augment-confer.r
NULL

#' @rdname stat-biplot-chull
#' @usage NULL
#' @export
StatChull <- ggproto(
  "StatChull", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  }
)

#' @rdname stat-biplot-chull
#' @export
stat_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA, 
  inherit.aes = TRUE,
  ...
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
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat-biplot-chull
#' @usage NULL
#' @export
StatUChull <- ggproto(
  "StatUChull", StatChull,
  
  setup_data = setup_u_data
)

#' @rdname stat-biplot-chull
#' @usage NULL
#' @export
StatVChull <- ggproto(
  "StatVChull", StatChull,
  
  setup_data = setup_v_data
)

#' @rdname stat-biplot-chull
#' @export
stat_u_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat-biplot-chull
#' @export
stat_v_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}
