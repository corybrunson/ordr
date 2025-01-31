ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = .25) +
  geom_lineranges()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = .25) +
  geom_pointranges(fun.data = mean_sdl, shape = "circle open")
