#' @title Bagplots
#'
#' @description Construct medians, bags, fences, and outlier specifications for
#'   bagplots.
#' 

#' @details A bagplot comprises a single, often filled, contour (the "bag")
#'   overlaid upon the [convex hull][stat_chull()] (the "fence") and a
#'   scatterplot of outliers (the "loop"). Rousseeuw &al (1999) also suggest the
#'   term "bag-and-bolster plot".
#' 

#' @template ref-rousseeuw1999

#' @template biplot-layers
#' @template biplot-ord-aes

#' @include stat-depth.r stat-chull.r
#' @inheritParams ggplot2::layer
#' @param median,fence,outliers Logical indicators whether to include median,
#'   fence, and outliers in the composite output.
#' @inheritDotParams stat_depth notion
#' @param fraction Fraction of the data to include in the bag.
#' @param coef Scale factor of the fence relative to the bag.
#' @example inst/examples/ex-stat-bagplot-judges.r
#' @example inst/examples/ex-stat-bagplot-iris.r
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
  
  extra_params = c("na.rm", "notion", "n"),
  
  compute_group = function(
    data, scales,
    fraction = 0.5, coef = 3,
    median = TRUE, fence = TRUE, outliers = TRUE,
    notion = "halfspace", n = 100,
    ...
  ) {
    # save(data, scales, fraction, coef, median, fence, outliers, notion, n,
    #      file = "stat-bagplot-compute-group.rda")
    # load(file = "stat-bagplot-compute-group.rda")
    
    # preserve throughout
    data_PANEL <- unique(data$PANEL)
    data_group <- unique(data$group)
    
    # compute `depth` column
    data$depth <- ddalpha::depth.(
      data[, c("x", "y")],
      data[, c("x", "y")],
      notion = notion
    )
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
      notion = notion, n = n
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
        get_hull() |> 
        as.matrix() |> 
        sweep(2L, as.matrix(median_df[, c("x", "y"), drop = FALSE]), "-") |> 
        magrittr::multiply_by(coef) |> 
        sweep(2L, as.matrix(median_df[, c("x", "y"), drop = FALSE]), "+") |> 
        as.data.frame() -> fence_hull
      
      # tag inliers & outliers
      outlying <- are_outlying(data, fence_hull)
      
    }
    
    # remove median and fence if not wanted
    if (! median) median_df <- data.frame()
    
    # end fence as the convex hull of the expanded bag hull and the inliers
    fence_df <- if (fence) {
      data |> 
        subset(! outlying, select = c("x", "y")) |> 
        rbind(fence_hull) |> 
        get_hull() |> 
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
    outliers_df <- if (outliers) {
      subset(data, outlying, select = c("x", "y"))
    } else {
      data.frame()
    }
    # tag the outliers
    if (nrow(outliers_df) > 0L) {
      outliers_df$component <- "outliers"
      outliers_df$PANEL <- data_PANEL
      outliers_df$group <- data_group
    }
    
    dplyr::bind_rows(median_df, bag_df, fence_df, outliers_df)
  }
)

get_hull <- function(data) data[grDevices::chull(data), , drop = FALSE]

# return subset of `data` that lies outside a convex polygon `hull`
# (don't forget the segment from the last to the first row)
# https://math.stackexchange.com/a/3441442
are_outlying <- function(data, hull) {
  
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
