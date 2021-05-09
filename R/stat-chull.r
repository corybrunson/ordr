#' @title Restrict geometric data to boundary points for its convex hull
#'
#' @description As used in a **[ggplot2][ggplot2::ggplot2]** vignette, this stat
#'   layer restricts a dataset with `x` and `y` variables to the points that lie
#'   on its convex hull. The biplot extension restricts each matrix factor to
#'   its own hull.
#'   

#' @template biplot-layers

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @family stat layers
#' @example inst/examples/ex-stat-chull-haireye.r
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

#' @rdname biplot-stats
#' @export
stat_rows_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRowsChull,
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

#' @rdname biplot-stats
#' @export
stat_cols_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsChull,
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

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatChull <- ggproto(
  "StatChull", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(
    data, scales
  ) {
    ord_cols <- get_ord_aes(data)
    
    data[chull(data[, ord_cols, drop = FALSE]), , drop = FALSE]
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsChull <- ggproto(
  "StatRowsChull", StatChull,
  
  setup_data = setup_rows_data
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsChull <- ggproto(
  "StatColsChull", StatChull,
  
  setup_data = setup_cols_data
)
