#' @title Bagplots
#'
#' @description Construct medians, bags, fences, and outlier specifications for
#'   bagplots.
#' 

#' @details A bagplot comprises a single, often filled, depth contour (the
#'   "bag") overlaid with the hull of its union with the data points contained
#'   in its scaled expansion from the depth median (the "fence") and a
#'   scatterplot of outliers beyond the fence (the "loop"). Rousseeuw &al (1999)
#'   suggest the term "bag-and-bolster plot" evocative of the "box-and-whisker
#'   plot".
#'
#'   While the depth median can be obtained using [stat_center()], the data
#'   depth values used to compute it are also used to demarcate the bag, so it
#'   is implemented separately in `StatBagplot$compute_group()` for efficiency.
#'
#'   `stat_bagplot()` is designed to pair with [geom_bagplot()], analogously to
#'   the pairing of [ggplot2::stat_boxplot()] with [ggplot2::geom_boxplot()]. In
#'   particular, `GeomBagplot` is the only `ggproto` that recognizes the
#'   computed variable `component`, used by `StatBagplot` to separate data for
#'   the four bagplot elements.
#' 

#' @template ref-rousseeuw1999

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`component`}{the component of the composite plot; used internally}
#' }

#' @include stat-depth.r stat-chull.r
#' @inheritParams ggplot2::layer
#' @param median,fence,outliers Logical indicators whether to include median,
#'   fence, and outliers in the composite output.
#' @inheritDotParams stat_depth notion notion_params
#' @param fraction Fraction of the data to include in the bag.
#' @param coef Scale factor of the fence relative to the bag.
#' @family stat layers
#' @example inst/examples/ex-stat-bagplot.r
#' @export
stat_bagplot <- function(
    mapping = NULL, data = NULL, geom = "bagplot", position = "identity",
    fraction = 0.5, coef = 3,
    median = TRUE, fence = TRUE, outliers = TRUE,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBagplot,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fraction = fraction, coef = coef,
      median = median, fence = fence, outliers = outliers,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBagplot <- ggproto(
  "StatBagplot", StatDepth,
  
  extra_params = c("na.rm", "notion", "notion_params", "n"),
  
  compute_group = function(
    data, scales,
    fraction = 0.5, coef = 3,
    median = TRUE, fence = TRUE, outliers = TRUE,
    notion = "zonoid", notion_params = list(), n = 100L,
    ...
  ) {
    # save(data, scales, fraction, coef, median, fence, outliers,
    #      notion, notion_params, n,
    #      file = "stat-bagplot-compute-group.rda")
    # load(file = "stat-bagplot-compute-group.rda")
    
    # preserve throughout
    data_PANEL <- unique(data$PANEL)
    data_group <- unique(data$group)
    
    # compute `depth` column
    depth_args <- list(
      x = data[, c("x", "y")],
      data = data[, c("x", "y")],
      notion = notion
    )
    depth_args <- c(depth_args, notion_params)
    data$depth <- do.call(what = ddalpha::depth., args = depth_args)
    # locate depth containing `fraction` of `data`
    z.intercept <- quantile(data$depth, probs = 1 - fraction)
    
    # locate the median at (the centroid of) the maximum density point(s)
    median_df <- if (median || fence) {
      data |> 
        subset(depth == max(depth), select = c("x", "y")) |> 
        lapply(mean) |> as.data.frame() |> 
        # cannot be empty so this will work
        transform(component = "median", PANEL = data_PANEL, group = data_group)
    } else {
      data.frame()
    }
    
    # compute `depth` grid
    depth_df <- StatDepth$compute_group(
      data, scales,
      notion = notion, notion_params = notion_params, n = n
    )
    depth_df$z <- depth_df$depth
    z.range <- range(depth_df$z, na.rm = TRUE, finite = TRUE)
    # compute bag (single contour) contour from grid
    bag_df <- StatContour$compute_group(
      depth_df, scales,
      z.range = z.range, breaks = z.intercept
    )
    # tag and remove any subgrouping of the bag (it being only one contour)
    bag_df$component <- "bag"
    bag_df$PANEL <- data_PANEL
    bag_df$group <- data_group
    
    # determine the fence hull and identify the outliers
    if (fence || outliers) {
      
      # begin fence by expanding the convex hull of the bag by `coef`
      bag_df |> 
        subset(select = c("x", "y")) |> 
        filter_hull() |> 
        as.matrix() |> 
        sweep(2L, as.matrix(median_df[, c("x", "y"), drop = FALSE]), "-") |> 
        magrittr::multiply_by(coef) |> 
        sweep(2L, as.matrix(median_df[, c("x", "y"), drop = FALSE]), "+") |> 
        as.data.frame() -> fence_hull
      
      # tag inliers & outliers
      outlying <- lie_without(data, fence_hull)
      
    }
    
    # remove median and fence if not wanted
    if (! median) median_df <- data.frame()
    
    # end fence as the convex hull of the expanded bag hull and the inliers
    fence_df <- if (fence) {
      data |> 
        subset(! outlying, select = c("x", "y")) |> 
        rbind(fence_hull) |> 
        filter_hull() |> 
        transform(component = "fence", PANEL = data_PANEL, group = data_group)
    } else {
      data.frame()
    }
    # tag the fence
    if (nrow(fence_df) > 0L) {
      fence_df$component <- "fence"
      fence_df$PANEL <- data_PANEL
      fence_df$group <- data_group
    }
    
    # identify the outliers
    outlier_df <- if (outliers) {
      subset(data, outlying, select = c("x", "y"))
    } else {
      data.frame()
    }
    # tag the outliers
    if (nrow(outlier_df) > 0L) {
      outlier_df$component <- "outliers"
      outlier_df$PANEL <- data_PANEL
      outlier_df$group <- data_group
    }
    
    dplyr::bind_rows(median_df, bag_df, fence_df, outlier_df)
  }
)

filter_hull <- function(data) data[grDevices::chull(data), , drop = FALSE]

# return subset of `data` that lies outside a convex polygon `hull`
# (don't forget the segment from the last to the first row)
# https://math.stackexchange.com/a/3441442
lie_without <- function(data, hull) {
  
  # iterate vertical ray crossings over edges (including from last to first row)
  # include points on the boundary
  winding <- 0L
  n <- nrow(hull)
  for (r in seq(n)) {
    # consecutive points along the hull
    x0 <- hull$x[(r - 1L) %% n + 1L]
    y0 <- hull$y[(r - 1L) %% n + 1L]
    x1 <- hull$x[r %% n + 1L]
    y1 <- hull$y[r %% n + 1L]
    m <- (y1 - y0) / (x1 - x0)
    
    winding <- winding + 
      if (x0 == x1) {
        # vertical line will not cross vertical ray
        0
      } else {
        # may assume `x0 != x1` i.e. `! is.na(m)`
        
        # intersection of segment and vertical line is above data
        ( m * (data$x - x0) + y0 > data$y ) *
          ( # edge crosses vertical line leftward
            (x0 >= data$x & data$x > x1) -
              # edge crosses vertical line rightward
              (x0 < data$x & data$x <= x1) )
      }
  }
  
  winding == 0L
}
