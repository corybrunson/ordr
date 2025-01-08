ggplot(mtcars, aes(x = wt, y = disp)) +
  geom_bagplot() +
  geom_point() +
  theme_bw()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_bagplot() +
  theme_bw()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  stat_bag() +
  theme_bw()
