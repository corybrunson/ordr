#' @title Construct limited rulers offset from the origin
#' 

#' @description Determine axis limits and offset vectors from reference data.
#' 

#' @template biplot-layers

#' @section Computed variables: These can be used with
#'   [`ggplot2::after_stat()`][ggplot2::aes_eval] to [control aesthetic
#'   evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).
#' \describe{
#'   \item{`x,y`}{cartesian coordinates (if passed polar)}
#'   \item{`angle/radius`}{polar coordinates (if passed cartesian)}
#'   \item{`lower,upper`}{distances of rule endpoints from origin}
#'   \item{`yintercept,xintercept`}{intercepts of offset axis}
#'   \item{`axis`}{unique axis identifier}
#' }

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param referent The point cloud to rule; a data frame with `x` and `y`
#'   columns or a subset of `c("rows", "cols")` to indicate row and/or column
#'   elements from an ordination model.
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-rule-glass.r
#' @export
stat_rule <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    .referent = NULL, referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
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
      .referent = .referent, referent = NULL,
      fun.min = fun.min, fun.max = fun.max,
      fun.offset = fun.offset,
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
    
    # TODO: Find a better way to handle this; are two parameters necessary?
    if (is.null(params$referent)) {
      # default to both row and column elements
      if (is.null(params$.referent)) {
        params$.referent <- c("rows", "cols")
      }
      # extract elements to referent
      params$referent <- 
        data[data$.matrix %in% params$.referent, c("x", "y"), drop = FALSE]
    } else {
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
    }
    # collapse to convex hull (specific to this statistical transformation)
    names(params$referent) <- c("x", "y")
    params$referent <- params$referent[chull(params$referent), , drop = FALSE]
    
    # if summary functions are `NULL` then use zero constant functions
    params$fun.min <- params$fun.min %||% const0
    params$fun.max <- params$fun.max %||% const0
    params$fun.offset <- params$fun.offset %||% const0
    
    params
  },
  
  setup_data = function(data, params) {
    
    data <- ensure_cartesian_polar(data, ggproto = "StatRule")
    
    setup_cols_data(data, params)
  },
  
  compute_group = function(
    data, scales,
    .referent = NULL, referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
    subset = NULL, elements = "all"
  ) {
    
    # include computed variables even if trivial
    if (is.null(referent)) {
      data <- transform(
        data,
        lower = -Inf, upper = Inf,
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
    group_vars <- c(names(data), "radius", "angle", "axis")
    data <- merge(data, referent, by = c())
    
    # compute projections of all referent points to each axis
    data |> 
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |> 
      # projections of referent points onto axis and its normal vector
      dplyr::mutate(
        h = radius_ * cos(angle_ - angle),
        v = radius_ * sin(angle_ - angle)
      ) |> 
      # compute offsets and endpoints
      dplyr::summarize(
        lower = fun.min(h),
        upper = fun.max(h),
        offset = fun.offset(v)
      ) |> 
      dplyr::ungroup() ->
      data
    data$h <- data$v <- NULL
    
    # additional computed variables
    data <- transform(
      data,
      # yintercept = offset / sin(angle + pi/2),
      # xintercept = offset / cos(angle + pi/2)
      xend = offset * cos(angle + pi/2),
      yend = offset * sin(angle + pi/2)
    )
    data$offset <- NULL
    
    data
  }
)

# convenience functions for rule limits and axis offset
const0 <- function(x) 0

#' @rdname stat_rule
#' @export
minpp <- function(x, p = .1) min(x) - diff(range(x)) * p

#' @rdname stat_rule
#' @export
maxpp <- function(x, p = .1) max(x) + diff(range(x)) * p

#' @rdname stat_rule
#' @export
minabspp <- function(x, p = .1) {
  minmaxpp <- c(minpp(x, p), maxpp(x, p))
  minmaxpp[which.min(abs(minmaxpp))]
}
