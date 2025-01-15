#' @title Depth estimates and contours
#' 
#' @description Estimate data depth using [ddalpha::depth.()].
#' 

#' @details Depth is an extension of the univariate notion of rank to bivariate
#'   (and sometimes multivariate) data (Rousseeuw &al, 1999). It comes in
#'   several flavors and is the basis for [bagplots][stat_bagplot()].
#'
#'   `stat_depth()` is adapted from [ggplot2::stat_density_2d()] and returns
#'   depth values over a grid in the same format, so it is neatly paired with
#'   [ggplot2::geom_contour()].
#' 

#' @template ref-rousseeuw1999

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#'
#'   `stat_depth()` and `stat_depth_filled()` compute different variables
#'   depending on whether contouring is turned on or off. With contouring off
#'   (`contour = FALSE`), both stats behave the same, and the following
#'   variables are provided:
#' \describe{
#'   \item{`depth`}{the depth estimate}
#'   \item{`ndepth`}{depth estimate, scaled to a maximum of 1}
#' }
#'   With contouring on (`contour = TRUE`), either [ggplot2::stat_contour()] or
#'   [ggplot2::stat_contour_filled()] is run after the depth estimate has been
#'   obtained, and the computed variables are determined by these stats.

#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_density_2d
#' @param contour If `TRUE`, contour the results of the depth estimation.
#' @param contour_var Character string identifying the variable to contour by.
#'   Can be one of `"depth"` or `"ndepth"`. See the section on computed
#'   variables for details.
#' @inheritDotParams ggplot2::geom_contour bins binwidth breaks
#' @param notion Character; the name of the depth function (passed to
#'   [ddalpha::depth.()]).
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-depth.r
#' @export
stat_depth <- function(
    mapping = NULL, data = NULL, geom = "contour", position = "identity",
    contour = TRUE, contour_var = "depth",
    notion = "halfspace",
    n = 100,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDepth,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      contour = contour, contour_var = contour_var,
      notion = notion,
      n = n,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat_depth
#' @export
stat_depth_filled <- function(
    mapping = NULL, data = NULL, geom = "contour_filled", position = "identity",
    contour = TRUE, contour_var = "depth",
    notion = "halfspace",
    n = 100,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDepthFilled,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      contour = contour, contour_var = contour_var,
      notion = notion,
      n = n,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDepth <- ggproto(
  "StatDepth", StatDensity2d,
  
  compute_layer = function(self, data, params, layout) {
    rlang::check_installed("ddalpha", reason = "for calculating depth.")
    # first run the regular layer calculation from which to infer depths
    data <- ggproto_parent(Stat, self)$compute_layer(data, params, layout)
    
    # if we're not contouring we're done
    if (!isTRUE(params$contour)) return(data)
    
    # set up data and parameters for contouring
    contour_var <- params$contour_var %||% "depth"
    match.arg(contour_var, c("depth", "ndepth"))
    data$z <- data[[contour_var]]
    z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params <- params[intersect(names(params), c("bins", "binwidth", "breaks"))]
    params$z.range <- z.range
    
    if (isTRUE(self$contour_type == "bands")) {
      contour_stat <- ggproto(NULL, StatContourFilled)
    } else { # lines is the default
      contour_stat <- ggproto(NULL, StatContour)
    }
    # update dropped aes
    contour_stat$dropped_aes <- c(contour_stat$dropped_aes, "depth", "ndepth")
    
    ggplot2:::dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1L])
      rlang::try_fetch(
        rlang::inject(
          contour_stat$compute_panel(data = data, scales = scales, !!! params)
        ),
        error = function() {
          warning("Computation failed in `stat_depth()`.")
          data.frame()
        }
      )
    })
  },
  
  compute_group = function(
    data, scales,
    notion = "halfspace",
    n = 100, ...
  ) {
    ord_cols <- get_ord_aes(data)
    notion <- match.arg(
      notion,
      # `eval(formals(ddalpha::depth.)$notion)`
      c("zonoid", "halfspace", "Mahalanobis", "projection", "spatial", 
        "spatialLocal", "simplicial", "simplicialVolume", "ddplot", 
        "potential")
    )
    
    # calculate depth
    x_ran <- scales$x$dimension()
    y_ran <- scales$y$dimension()
    xy_grid <- expand.grid(
      x = seq(x_ran[1L], x_ran[2L], length.out = n),
      y = seq(y_ran[1L], y_ran[2L], length.out = n)
    )
    depth <- ddalpha::depth.(
      xy_grid,
      data[, ord_cols[seq(2L)], drop = FALSE],
      notion = notion
    )
    
    # prepare final output data frame
    df <- xy_grid
    df$depth <- depth
    df$group <- data$group[1L]
    df$ndepth <- df$depth / max(df$depth, na.rm = TRUE)
    df$n <- nrow(data) # number of observations in this group
    df$level <- 1
    df$piece <- 1
    df
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDepthFilled <- ggproto(
  "StatDepthFilled", StatDepth,
  default_aes = aes(colour = NA, fill = after_stat(level)),
  contour_type = "bands"
)
