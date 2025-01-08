ggplot(mtcars, aes(x = wt, y = disp)) +
  geom_bagplot(coef = 1.5) +
  theme_bw()

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_bagplot(fence = FALSE) +
  theme_bw()
