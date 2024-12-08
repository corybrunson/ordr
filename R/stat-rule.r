#' @title Construct limited rulers offset from the origin
#' 

#' @description Determine axis limits and offset vectors from reference data.
#' 

#' @details
#'
#' Biplots with several axes can become cluttered and illegible. When this
#' happens, Gower, Gardner--Lubbe, & le Roux (2011) recommend to translate the
#' axes to a new point of intersection away from the origin, adjusting the axis
#' markers accordingly. Then the axes converge in a region of the plot offset
#' from most position markers or other elements. An alternative solution,
#' implemented in the **[bipl5][bipl5]** package, is to translate each axis
#' orthogonally away from the origin, which preserves the axis markers. This is
#' the technique implemented here.
#'
#' Separately, axes that fill the plotting window are uninformative when they
#' exceed the range of the plotted position markers projected onto them. They
#' may even be misinformative, suggesting that linear relationships extrapolate
#' outside the data range. In these cases, Gower and Harding (1988) recommend
#' using finite ranges determined by the data projection onto each axis.
#'
#' Three functions control these operations: `fun.offset` computes the
#' orthogonal distance of each axis from the origin, and `fun.lower` and
#' `fun.upper` compute the distance along each axis of the endpoints to the
#' (offset) origin. Both functions depend on what position data is to be offset
#' from or limited to, which must be either passed manually to the `referent`
#' parameter or encoded as named matrix factors to the helper parameter
#' `.referent`.
#' 

#' @template ref-gower2011
#' @template ref-gower1988
#'   

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`x,y`}{cartesian coordinates (if passed polar)}
#'   \item{`angle,radius`}{polar coordinates (if passed cartesian)}
#'   \item{`lower,upper`}{distances to endpoints from origin (before offset)}
#'   \item{`yintercept,xintercept`}{intercepts (possibly `Inf`) of offset axis}
#'   \item{`axis`}{unique axis identifier (integer)}
#' }

#' @inheritParams ggplot2::layer
#' @inheritParams stat_center
#' @template param-stat
#' @param .referent A character string indicating the matrix factor(s) of an
#'   ordination model to include in `referent`, handled the same way as
#'   `.matrix`.
#' @param referent The point cloud to rule; a data frame with `x` and `y`
#'   columns.
#' @param fun.lower,fun.upper,fun.offset Functions used to determine the limits
#'   of the rules and the translations of the axes from the projections of
#'   `referent` onto the axes and onto their normal vectors.
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-rule-glass.r
#' @export
stat_rule <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    .referent = NULL, referent = NULL,
    fun.lower = "minpp", fun.upper = "maxpp",
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
      fun.lower = fun.lower, fun.upper = fun.upper,
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
        params$.referent <- "dims"
      }
      params$.referent <- match_factor(params$.referent)
      # which elements to extract
      facs <- if (params$.referent == "dims") 
        c("rows", "cols") 
      else 
        params$.referent
      # extract elements to referent
      params$referent <- data[data$.matrix %in% facs, c("x", "y"), drop = FALSE]
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
    fun.lower = "minpp", fun.upper = "maxpp",
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
    lofun <- make_limits_offset_fun(fun.lower, fun.upper, fun.offset, fun.args)
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

# take `fun.lower,fun.upper` and return a function that summarizes a data frame
make_limits_offset_fun <- function(fun.lower, fun.upper, fun.offset, fun.args) {
  force(fun.lower)
  force(fun.upper)
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
  if (is.null(fun.lower) && is.null(fun.upper)) {
    fun_limits <- fun_null
  } else {
    if (! is.null(fun.lower) || ! is.null(fun.upper)) {
      # if either range limit is `NULL`, set it to the constant zero function
      # TODO: If either range limit is `NULL`, make it the reverse of the other?
      if (is.null(fun.lower)) {
        fun.upper <- match.fun(fun.upper)
        # fun.lower <- \(x) x[which(-x == fun.upper(-x))[1L]]
        fun.lower <- const0
      } else if (is.null(fun.upper)) {
        fun.lower <- match.fun(fun.lower)
        # fun.upper <- \(x) x[which(-x == fun.lower(-x))[1L]]
        fun.upper <- const0
      }
    }
    # both limits
    fun_limits <- function(df, ...) {
      data.frame(
        lower = call_fun(fun.lower, df$h),
        upper = call_fun(fun.upper, df$h)
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
