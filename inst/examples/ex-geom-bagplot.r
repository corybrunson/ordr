ggplot(mtcars, aes(x = wt, y = disp)) +
  geom_bagplot() +
  geom_point() +
  theme_bw()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  stat_bagplot() +
  theme_bw()
