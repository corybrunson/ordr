ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = .25) +
  geom_lineranges()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = .25) +
  geom_pointranges(fun.data = mean_sdl, shape = "circle open")

mpg %>%
  aggregate(
    x = cbind(displ, hwy) ~ 0,
    FUN = function(z) c(min = min(z), med = median(z), max = max(z))
  ) %>%
  do.call(what = data.frame) %>%
  ggplot(aes(displ.med, hwy.med)) +
  geom_pointranges(
    stat = "identity",
    aes(xmin = displ.min, xmax = displ.max, ymin = hwy.min, ymax = hwy.max)
  ) +
  geom_point(data = mpg, aes(displ, hwy), alpha = .5)
