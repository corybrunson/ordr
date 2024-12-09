# cartesian coordinates
us_center <- sapply(state.center, \(x) (min(x) + max(x)) / 2)
state_center <- cbind(
  state = state.abb,
  sweep(as.data.frame(state.center), 2, us_center, "-")
)
ggplot(state_center, aes(x, y, label = state)) +
  coord_equal() +
  geom_vector() +
  geom_text_radiate()
# polar coordinates
height_corr <- data.frame(
  measurement = colnames(Harman23.cor$cov),
  correlation = Harman23.cor$cov[1, ]
)
ggplot(height_corr, aes(angle = acos(correlation), label = measurement)) +
  coord_equal() +
  geom_vector(radius = 1) +
  geom_text_radiate(radius = 1) +
  expand_limits(x = c(NA, 1.5), y = c(NA, 1.5))
