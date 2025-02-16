# resize the plot to see that the specified aspect ratio is maintained
p <- ggplot(mtcars, aes(mpg, hp/10)) + geom_point()
p + coord_scaffold()
p + coord_scaffold(window_ratio = 2)

# prevent rescaling in response to `theme()` aspect ratio
p <- ggplot(mtcars, aes(mpg, hp/5)) + geom_point()
p + coord_equal() + theme(aspect.ratio = 1)
p + coord_scaffold() + theme(aspect.ratio = 1)

# NB: `theme(aspect.ratio = )` overrides `Coord*$aspect`:
p + coord_fixed(ratio = 1) + theme(aspect.ratio = 1)
p + coord_scaffold(window_ratio = 2) + theme(aspect.ratio = 1)
