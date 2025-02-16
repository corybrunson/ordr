ggplot(mpg, aes(x = displ, y = cty, shape = drv)) +
  geom_point() +
  stat_center(fun = "median", size = 5, alpha = .5)

ggplot(mpg, aes(x = displ, y = cty, shape = drv, linetype = drv)) +
  stat_center(size = 3) +
  stat_star()
