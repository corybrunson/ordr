# base Motor Trends plot
b <- ggplot(mtcars, aes(wt, disp)) + geom_point()

# depth raster
b + geom_raster(stat = "depth", aes(fill = after_stat(depth)))
# depth grid
b + stat_depth(
  geom = "point", contour = FALSE,
  aes(size = after_stat(depth)), n = 20
)

# depth contours
b + geom_contour(stat = "depth", contour = TRUE)
# depth bands
b + geom_contour_filled(stat = "depth_filled", contour = TRUE, alpha = .75)
# contours colored by group
b + stat_depth(aes(color = factor(cyl)))
# custom depth notion
b + stat_depth(
  aes(color = factor(cyl)),
  notion = "halfspace", notion_params = list(exact = TRUE)
)

# contours faceted by group
b + stat_depth_filled(alpha = .75) +
  facet_wrap(facets = vars(factor(cyl)))
# scaled to the unit interval
b + stat_depth_filled(contour_var = "ndepth", alpha = .75) +
  facet_wrap(facets = vars(factor(cyl)))
