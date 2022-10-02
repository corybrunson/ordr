#' @title Compute geometric centers and spreads for ordination factors
#'

#' @template biplot-layers
#' @template biplot-ord-aes

#' @inheritParams ggplot2::layer
#' @param fun.data,fun.center,fun.min,fun.max,fun.args Functions and arguments
#'   treated as in [ggplot2::stat_summary()], with `fun.center`, `fun.min`, and
#'   `fun.max` behaving as `fun.y`, `fun.ymin`, and `fun.ymax`.
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-center-iris.r
#' @export
stat_center <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.min = NULL,
  fun.max = NULL,
  fun.args = list()
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
      fun.data = fun.data,
      fun.center = fun.center,
      fun.min = fun.min,
      fun.max = fun.max,
      fun.args = fun.args,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCenter <- ggproto(
  "StatCenter", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           fun.data = NULL,
                           fun.center = NULL, fun.min = NULL, fun.max = NULL,
                           fun.args = list(),
                           na.rm = FALSE) {
    cfun <- make_center_fun(fun.data, fun.center, fun.min, fun.max, fun.args)
    cfun(data)
  }
)

#' @rdname stat_center
#' @export
stat_star <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.args = list()
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatStar,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      fun.center = fun.center,
      fun.args = fun.args,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatStar <- ggproto(
  "StatStar", StatCenter,
  
  compute_group = function(data, scales,
                           fun.data = NULL,
                           fun.center = NULL, fun.args = list(),
                           na.rm = FALSE) {
    cfun <- make_center_fun(fun.data, fun.center, NULL, NULL, fun.args)
    cdata <- cfun(data)
    
    data$xend <- data$x
    data$yend <- data$y
    data$x <- cdata$x
    data$y <- cdata$y
    
    data
  }
)

make_center_fun <- function(fun.data, fun.center, fun.min, fun.max, fun.args) {
  force(fun.data)
  force(fun.center)
  force(fun.min)
  force(fun.max)
  force(fun.args)
  
  if (! is.null(fun.data)) {
    # single data summary function
    
    fun.data.y <- match.fun(fun.data)
    fun.data.x <- function(x, ...) {
      .data <- fun.data.y(x, ...)
      names(.data) <- c("x", "xmin", "xmax")
      .data
    }
    function(df) {
      x_data <- do.call(fun.data.x, c(list(quote(df$x)), fun.args))
      y_data <- do.call(fun.data.y, c(list(quote(df$y)), fun.args))
      cbind(x_data, y_data)
    }
  } else if (! is.null(fun.center)) {
    # separate vector summary functions
    
    call_fun <- function(fun, x) {
      if (is.null(fun)) return(NA_real_)
      do.call(fun, c(list(quote(x)), fun.args))
    }
    
    fun.center <- match.fun(fun.center)
    if (is.null(fun.min) && is.null(fun.max)) {
      # center function only
      
      function(df, ...) {
        data.frame(
          x = call_fun(fun.center, df$x),
          y = call_fun(fun.center, df$y)
        )
      }
    } else {
      # center and limit functions
      
      # if either range limit is `NULL`, set it to the other
      if (is.null(fun.min)) {
        fun.min <- fun.max
      } else if (is.null(fun.max)) {
        fun.max <- fun.min
      }
      fun.min <- match.fun(fun.min)
      fun.max <- match.fun(fun.max)
      function(df, ...) {
        data.frame(
          x = call_fun(fun.center, df$x),
          xmin = call_fun(fun.min, df$x),
          xmax = call_fun(fun.max, df$x),
          y = call_fun(fun.center, df$y),
          ymin = call_fun(fun.min, df$y),
          ymax = call_fun(fun.max, df$y)
        )
      }
    }
  } else {
    message("No center (limit) function(s) supplied; defaulting to `mean_se()`")
    function(df) {
      x_data <- mean_se(df$x)
      names(x_data) <- c("x", "xmin", "xmax")
      y_data <- mean_se(df$y)
      cbind(x_data, y_data)
    }
  }
}
