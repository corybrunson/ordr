#' Compute geometric centers for ordination factors
#'

#' @template ggbiplot-layers

#' @name ggbiplot-center
#' @inheritParams ggplot2::layer
#' @param fun A function that summarizes a numeric vector, or else the character
#'   name of a pre-defined function (\code{"mean"} or \code{"median"}).
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.
#' @example inst/examples/ex-iris.r

#' @rdname ggbiplot-center
#' @usage NULL
#' @export
StatCenter <- ggproto(
  "StatCenter", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, fun = mean, na.rm = FALSE) {
    if (is.character(fun)) fun <- match.fun(fun)
    data.frame(x = fun(data$x, na.rm = na.rm), y = fun(data$y, na.rm = na.rm))
  }
)

#' @rdname ggbiplot-center
#' @export
stat_center <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun = "mean"
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCenter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-center
#' @usage NULL
#' @export
StatUCenter <- ggproto(
  "StatUCenter", StatCenter,
  
  setup_data = function(data, params) {
    data[data$.matrix == "u", -match(".matrix", names(data)), drop = FALSE]
  }
)

#' @rdname ggbiplot-center
#' @usage NULL
#' @export
StatVCenter <- ggproto(
  "StatVCenter", StatCenter,
  
  setup_data = function(data, params) {
    data[data$.matrix == "v", -match(".matrix", names(data)), drop = FALSE]
  }
)

#' @rdname ggbiplot-center
#' @export
stat_u_center <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun = "mean"
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUCenter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-center
#' @export
stat_v_center <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun = "mean"
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVCenter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      na.rm = FALSE,
      ...
    )
  )
}
