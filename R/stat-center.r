#' @title Compute geometric centers and spreads for ordination factors
#'

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`xmin,ymin,xmax,ymax`}{results of `fun.min,fun.max` applied to `x,y`}
#' }

#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_summary_bin
#' @param fun.center Deprecated alias to `fun`.
#' @param fun.ord Alternatively to the [ggplot2::stat_summary_bin()] parameters,
#'   supply a summary function that takes a matrix as input and returns a named
#'   column summary vector. Names must include be those of the input columns and
#'   may also include their `*min` and `*max` counterparts. Overridden by
#'   `fun.data` and `fun`, cannot be used together with `fun.min` and `fun.max`.
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-center.r
#' @export
stat_center <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun = NULL, fun.center = NULL,
  fun.min = NULL,
  fun.max = NULL,
  fun.ord = NULL,
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
      fun = fun, fun.center = fun.center,
      fun.min = fun.min,
      fun.max = fun.max,
      fun.ord = fun.ord,
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
  
  setup_params = function(data, params) {
    
    # deprecate `fun.center`
    if (! is.null(params$fun.center)) {
      if (is.null(params$fun)) {
        warning("`fun.center` is deprecated; use `fun` instead.")
        params$fun <- params$fun.center
      } else {
        warning("`fun` will be used instead of `fun.center`.")
      }
      params$fun.center <- NULL
    }
    
    params
  },
  
  extra_params = c("fun.center"),
  
  compute_group = function(data, scales,
                           fun.data = NULL,
                           fun = NULL,
                           fun.min = NULL, fun.max = NULL,
                           fun.ord = NULL,
                           fun.args = list(),
                           na.rm = FALSE) {
    ord_cols <- get_ord_aes(data)
    cfun <- 
      make_center_fun(fun.data, fun, fun.min, fun.max, fun.ord, fun.args)
    cfun(data[, ord_cols, drop = FALSE])
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
  fun = NULL, fun.center = NULL,
  fun.ord = NULL,
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
      fun = fun, fun.center = fun.center,
      fun.ord = fun.ord,
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
                           fun.data = NULL, fun = NULL,
                           fun.ord = NULL, fun.args = list(),
                           na.rm = FALSE) {
    ord_cols <- get_ord_aes(data)
    cfun <- make_center_fun(fun.data, fun, NULL, NULL, fun.ord, fun.args)
    cdata <- cfun(data[, ord_cols, drop = FALSE])
    
    data$xend <- data$x
    data$yend <- data$y
    data$x <- cdata$x
    data$y <- cdata$y
    
    data
  }
)

make_center_fun <- function(
    fun.data, fun, fun.min, fun.max, fun.ord, fun.args
) {
  force(fun.data)
  force(fun)
  force(fun.min)
  force(fun.max)
  force(fun.ord)
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
    
  } else if (! is.null(fun)) {
    # separate vector summary functions
    
    call_fun <- function(fun, x) {
      if (is.null(fun)) return(NA_real_)
      do.call(fun, c(list(quote(x)), fun.args))
    }
    
    fun <- match.fun(fun)
    if (is.null(fun.min) && is.null(fun.max)) {
      # center function only
      
      function(df, ...) {
        data.frame(
          x = call_fun(fun, df$x),
          y = call_fun(fun, df$y)
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
      function(df) {
        data.frame(
          x = call_fun(fun, df$x),
          xmin = call_fun(fun.min, df$x),
          xmax = call_fun(fun.max, df$x),
          y = call_fun(fun, df$y),
          ymin = call_fun(fun.min, df$y),
          ymax = call_fun(fun.max, df$y)
        )
      }
    }
    
  } else if (! is.null(fun.ord)) {
    # multivariable summary function
    
    fun.ord <- match.fun(fun.ord)
    
    function(df) {
      x <- fun.ord(df)
      as.data.frame(as.list(x))
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

depth_median <- function(x, notion = "halfspace", ...) {
  x <- as.matrix(x)
  d <- ddalpha::depth.(x, x, notion = notion)
  i <- which(d == max(d))
  apply(x[i, , drop = FALSE], 2L, mean, na.rm = FALSE, simplify = TRUE)
}
