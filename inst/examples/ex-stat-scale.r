d <- data.frame(x = c(1, 0), y = c(0, 1))
ggplot(d, aes(x, y)) +
  geom_point(size = 3) +
  geom_vector(stat = "scale", mult = 2)
