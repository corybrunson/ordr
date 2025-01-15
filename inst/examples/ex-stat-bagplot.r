# petroleum rock base plot
p <- ggplot(rock, aes(area, shape, size = peri)) + theme_bw()
# scatterplot
p + geom_point()
# custom bag fraction
p + stat_bagplot(fraction = .5, coef = 2)
# custom fence coefficient
p + stat_bagplot(coef = 1.5)
# trivial fence
p + stat_bagplot(coef = 1)
# invisible fence
p + stat_bagplot(fence = FALSE)
