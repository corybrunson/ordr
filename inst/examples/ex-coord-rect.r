# ensures that the resolutions of the axes and the dimensions of the plotting
# window respect the specified aspect ratios
p <- ggplot(mtcars, aes(mpg, hp/10)) + geom_point()
p + coord_rect(ratio = 1)
p + coord_rect(ratio = 1, window_ratio = 2)
p + coord_rect(ratio = 1, window_ratio = 1/2)
p + coord_rect(ratio = 5)
p + coord_rect(ratio = 1/5)
p + coord_rect(xlim = c(15, 30))
p + coord_rect(ylim = c(15, 30))
