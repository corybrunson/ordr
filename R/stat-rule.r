#' @title Construct limited rulers offset from the origin
#' 

#' @description Determine axis limits and offset vectors from reference data.
#' 

#' @template biplot-layers

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`x,y`}{cartesian coordinates (if passed polar)}
#'   \item{`angle,radius`}{polar coordinates (if passed cartesian)}
#'   \item{`lower,upper`}{distances of rule endpoints from origin}
#'   \item{`yintercept,xintercept`}{intercepts of offset axis}
#'   \item{`axis`}{unique axis identifier}
#' }

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param .referent A character vector indicating the matrix factor(s) of an
#'   ordination model to include in `referent`; should be a subset of `c("rows",
#'   "cols")`.
#' @param referent The point cloud to rule; a data frame with `x` and `y`
#'   columns.
#' @param fun.min,fun.max Functions used to determine the limits of rules from
#'   the projections of `referent` onto the axes.
#' @param fun.offset Function used to determine the directions and magnitudes of
#'   the axis offsets from the projections of `referent` onto the normal vectors
#'   of the axes.
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-rule-glass.r
#' @export
stat_rule <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    .referent = NULL, referent = NULL,
    fun.min = "minpp", fun.max = "maxpp",
    fun.offset = "minabspp",
    fun.args = list(),
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent, referent = referent,
      fun.min = fun.min, fun.max = fun.max,
      fun.offset = fun.offset,
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
StatRule <- ggproto(
  "StatRule", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {

    # TODO: Reliably check that a data frame was fortified from a 'tbl_ord'.
    from_tbl_ord <- ".matrix" %in% names(data)
    # TODO: Find a better way to handle this; are two parameters necessary?
    if (! from_tbl_ord) {
      if (! is.null(params$.referent))
        warning("Only use `.referent` to specify ordination matrix factors.")
    }
    if (from_tbl_ord && is.null(params$referent)) {
      # default to both row and column elements
      if (is.null(params$.referent)) {
        params$.referent <- c("rows", "cols")
      }
      # extract elements to referent
      params$referent <- 
        data[data$.matrix %in% params$.referent, c("x", "y"), drop = FALSE]
    } else if (! is.null(params$referent)) {
      # require coordinate data
      stopifnot(
        is.data.frame(params$referent) || is.matrix(params$referent),
        ncol(params$referent) == 2L,
        all(apply(params$referent, 2L, is.numeric))
      )
      params$referent <- as.data.frame(params$referent)
      if (! is.null(params$.referent)) {
        warning("`referent` was provided; `.referent` will be ignored.")
        params$.referent <- NULL
      }
      names(params$referent) <- c("x", "y")
    }
    # collapse to convex hull (specific to this statistical transformation)
    if (! is.null(params$referent))
      params$referent <- params$referent[chull(params$referent), , drop = FALSE]
    
    params
  },
  
  setup_data = function(data, params) {
    
    data <- ensure_cartesian_polar(data)
    
    data
  },
  
  compute_group = function(
    data, scales,
    .referent = NULL, referent = NULL,
    fun.min = "minpp", fun.max = "maxpp",
    fun.offset = "minabspp",
    fun.args = list(),
    subset = NULL, elements = "all"
  ) {
    
    # include computed variables even if trivial
    if (is.null(referent)) {
      data <- transform(
        data,
        # lower = -Inf, upper = Inf,
        yintercept = 0, xintercept = 0
      )
      
      return(data)
    }
    
    # prepare elements and referents for projection calculations
    names(referent) <- c("x_", "y_")
    referent <- transform(
      referent,
      radius_ = sqrt(x_^2 + y_^2),
      angle_ = atan2(y_, x_)
    )
    data <- transform(
      data,
      radius = sqrt(x^2 + y^2),
      angle = atan2(y, x),
      axis = seq(nrow(data))
    )
    group_vars <- names(data)
    data <- merge(data, referent, by = c())
    
    # compute horizontal and vertical projections of referent points onto axes
    data <- transform(
      data,
      h = radius_ * cos(angle_ - angle),
      v = radius_ * sin(angle_ - angle)
    )
    
    # compute limits and offsets
    lofun <- make_limits_offset_fun(fun.min, fun.max, fun.offset, fun.args)
    data <- tidyr::nest(data, df = -tidyselect::all_of(group_vars))
    data$df <- lapply(data$df, lofun)
    data <- tidyr::unnest(data, df)
    
    # additional computed variables
    if (! is.null(data[["offset"]])) {
      data <- transform(
        data,
        # yintercept = offset / sin(angle + pi/2),
        # xintercept = offset / cos(angle + pi/2)
        xend = offset * cos(angle + pi/2),
        yend = offset * sin(angle + pi/2)
      )
      data$offset <- NULL
    }
    
    data
  }
)

const0 <- function(x) 0

#' @rdname stat_rule
#' @export
minpp <- function(x, p = .1) min(x) - diff(range(x)) * p

#' @rdname stat_rule
#' @export
maxpp <- function(x, p = .1) max(x) + diff(range(x)) * p

#' @rdname stat_rule
#' @param x A numeric vector.
#' @param p A numeric value; the proportion of a range used as a buffer.
#' @export
minabspp <- function(x, p = .1) {
  minmaxpp <- c(minpp(x, p), maxpp(x, p))
  minmaxpp[which.min(abs(minmaxpp))]
}

# take `fun.min` & `fun.max` and return a function that summarizes a data frame
make_limits_offset_fun <- function(fun.min, fun.max, fun.offset, fun.args) {
  force(fun.min)
  force(fun.max)
  force(fun.offset)
  force(fun.args)
  
  call_fun <- function(fun, x) {
    if (is.null(fun)) return(NA_real_)
    do.call(fun, c(list(quote(x)), fun.args))
  }
  # null function; empty 1-row data frame
  fun_null <- function(df, ...) {
    as.data.frame(matrix(NA_real_, nrow = 1L, ncol = 0L))
  }
  
  # limits function
  if (is.null(fun.min) && is.null(fun.max)) {
    fun_limits <- fun_null
  } else {
    if (! is.null(fun.min) || ! is.null(fun.max)) {
      # if either range limit is `NULL`, set it to the constant zero function
      # TODO: If either range limit is `NULL`, make it the reverse of the other?
      if (is.null(fun.min)) {
        fun.max <- match.fun(fun.max)
        # fun.min <- \(x) x[which(-x == fun.max(-x))[1L]]
        fun.min <- const0
      } else if (is.null(fun.max)) {
        fun.min <- match.fun(fun.min)
        # fun.max <- \(x) x[which(-x == fun.min(-x))[1L]]
        fun.max <- const0
      }
    }
    # both limits
    fun_limits <- function(df, ...) {
      data.frame(
        lower = call_fun(fun.min, df$h),
        upper = call_fun(fun.max, df$h)
      )
    }
  }
  
  # offset function
  if (is.null(fun.offset)) {
    fun_offset <- fun_null
  } else {
    fun.offset <- match.fun(fun.offset)
    fun_offset <- function(df, ...) {
      data.frame(
        offset = call_fun(fun.offset, df$v)
      )
    }
  }
  
  # combined function
  function(df, ...) {
    cbind(fun_limits(df, ...), fun_offset(df, ...))
  }
}
