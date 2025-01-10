# petroleum rock base plot
p <- ggplot(rock, aes(area, shape, size = peri)) + theme_bw()
# scatterplot
p + geom_point()
# custom bag fraction
# FIXME: Bag self-intersects.
# FIXME: `size` aesthetic does not affect median or outliers.
p + stat_bagplot(fraction = .75)
# custom fence coefficient
p + stat_bagplot(coef = 1.5)
# trivial fence
p + stat_bagplot(coef = 1)
# invisible fence
p + stat_bagplot(fence = FALSE)
