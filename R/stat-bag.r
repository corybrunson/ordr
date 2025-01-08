#' @title Bagplots
#'
#' @description Construct medians, bags, fences, and outlier specifications for
#'   bagplots.
#' 

#' @details A bagplot comprises a single, often filled, contour (the "bag")
#'   overlaid upon the [convex hull][stat_chull()] (the "fence") and a
#'   scatterplot of outliers (the "loop"). Reusseeuw &al (1999) also suggest the
#'   term "bag-and-bolster plot".
#' 

#' @template ref-rousseeuw1999

#' @template biplot-layers
#' @template biplot-ord-aes

#' @include stat-depth.r stat-chull.r
#' @param bag_var The stat to use to construct the bag; one of `"depth"`,
#'   `"density"`, and `"chull"`.
#' @param median,fence,outliers Logical indicators whether to include median,
#'   fence, and outliers in the composite output.
#' @inheritDotParams stat_depth notion
#' @inheritDotParams ggplot2::stat_density_2d h adjust n
#' @param fraction Fraction of the data to include in the bag.
#' @example inst/examples/ex-stat-bag-judges.r
#' @example inst/examples/ex-stat-bag-iris.r
#' @export
stat_bag <- function(
    mapping = NULL, data = NULL, geom = "bag", position = "identity",
    bag_var = "depth", fraction = 0.5,
    median = TRUE, fence = TRUE, outliers = TRUE,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBag,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bag_var = bag_var, fraction = fraction,
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
StatBag <- ggproto(
  "StatBag", StatDepth,
  
  extra_params = c("na.rm", "notion", "h", "adjust", "n"),
  
  # TODO: Complicate this once the prototype is working.
  setup_params = function(data, params) {
    
    params$bag_var <- match.arg(params$bag_var, c("depth", "density", "chull"))
    
    params
  },
  
  compute_group = function(
    data, scales,
    bag_var = "depth", fraction = 0.5,
    median = TRUE, fence = TRUE, outliers = TRUE,
    notion = "halfspace", h = NULL, adjust = c(1, 1), n = 100,
    ...
  ) {
    # TODO: Assemble and annotate data for median, bag, fence, and outliers.
    # TODO: Call `Stat*` to estimate z across a grid (unless chull).
    # save(data, scales, bag_var, fraction, median, fence, outliers,
    #      notion, h, adjust, n,
    #      file = "stat-bag-compute-group.rda")
    # load(file = "stat-bag-compute-group.rda")
    
    # preserve throughout
    data_PANEL <- unique(data$PANEL)
    data_group <- unique(data$group)
    
    # compute bag and median
    if (bag_var == "depth") {
      
      # compute `depth` column
      data <- get_depths(data, notion = notion)
      z.intercept <- quantile(data$depth, probs = 1 - fraction)
      
      # locate the median at (the centroid of) the maximum density point(s)
      if (median) median_df <- subset(data, depth == max(depth))
      
      depth_df <- StatDepth$compute_group(
        data, scales,
        notion = notion, n = n
      )
      # TODO: Use `ndepth` if necessary.
      # contour_var <- params$contour_var %||% "depth"
      # match.arg(contour_var, c("depth", "ndepth"))
      depth_df$z <- depth_df$depth
      z.range <- range(depth_df$z, na.rm = TRUE, finite = TRUE)
      
      bag_df <- StatContour$compute_group(
        depth_df, scales,
        z.range = z.range, breaks = z.intercept
      )
      
    } else if (bag_var == "density") {
      
      dens_df <- StatDensity2d$compute_group(
        data, scales,
        h = h, adjust = adjust, n = n
      )
      # TODO: Use `ndensity` if necessary.
      # contour_var <- params$contour_var %||% "density"
      # match.arg(contour_var, c("density", "ndensity"))
      dens_df$z <- dens_df$density
      z.range <- range(dens_df$z, na.rm = TRUE, finite = TRUE)
      
      # locate the median at (the centroid of) the maximum density point(s)
      if (median) {
        warning("Median is not defined for density.")
        median_df <- 
          data.frame(x = numeric(0), y = numeric(0), component = character(0))
      }
      
      # FIXME: Treat density like depth above, or else remove this option.
      bag_df <- StatContour$compute_group(
        dens_df, scales,
        z.range = z.range, breaks = fraction
      )
      
    } else if (bag_var == "chull") {
      
      # also obtain the median as the centroid of the innermost nonempty hull
      breaks <- c(fraction, 0, nonempty = TRUE)
      peel_df <- StatPeel$compute_group(
        data, scales,
        breaks = breaks, cut = cut
      )
      
      if (median) median_df <- subset(peel_df, hull == 2L)
      
      bag_df <- subset(peel_df, hull == 1L)
    }
    
    # remove any subgrouping of the bag (this is only one contour)
    bag_df <- transform(bag_df, PANEL = data_PANEL, group = data_group)
    
    # tag the bag
    if (nrow(bag_df) > 0L) bag_df$component <- "bag"
    
    # collapse and tag the median
    median_df <- if (median && nrow(median_df) > 0L) {
      lapply(median_df[, c("x", "y")], mean) |> 
        as.data.frame() |> 
        transform(component = "median", PANEL = data_PANEL, group = data_group)
    } else {
      data.frame()
    }
    
    # compute and tag the fence (without being clever above)
    fence_df <- if (fence) {
      StatChull$compute_group(data, scales) |> 
        transform(component = "fence", PANEL = data_PANEL, group = data_group)
    } else {
      data.frame()
    }

    # identify and tag the outliers
    outlier_df <- if (outliers) {
      # https://math.stackexchange.com/a/3441442
      get_outliers(
        data = subset(data, select = c(x, y)),
        bag = subset(bag_df, select = c(x, y))
      )
    } else {
      data.frame()
    }
    if (nrow(outlier_df) > 0L) {
      outlier_df <- 
        transform(outlier_df, PANEL = data_PANEL, group = data_group)
    }
    
    dplyr::bind_rows(median_df, bag_df, fence_df, outlier_df)
  }
)

# return subset of `data` at maximum depth
get_depths <- function(data, notion) {
  depths <- ddalpha::depth.(
    data[, c("x", "y")],
    data[, c("x", "y")],
    notion = notion
  )
  transform(data, depth = depths)
}

# return subset of `data` that lies outside the polygon traced by `bag`
# (don't forget the segment from the last to the first row of `bag`)
get_outliers <- function(data, bag) {
  
  # replace `bag` by its convex hull
  # TODO: Only allow this if density is excluded.
  bag <- bag[grDevices::chull(bag), , drop = FALSE]
  # for the last segment
  bag <- rbind(bag, bag[1L, , drop = FALSE])
  
  # initialize winding number
  data$winding <- 0L
  
  # iterate vertical ray crossings over edges
  for (r in seq(nrow(bag) - 1L)) {
    data$winding <- data$winding + 
      (bag$x[r] >= data$x & data$x > bag$x[r+1L]) -
      (bag$x[r] <= data$x & data$x < bag$x[r+1L])
  }
  
  data[data$winding != 0, setdiff(names(data), "winding")]
}
