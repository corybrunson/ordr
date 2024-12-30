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
# using arrowhead to encode a variable
state_center$coast <- ifelse(
  state.region == "North Central",
  NA,
  state.region == "West"
)
ggplot(state_center, aes(x, y, label = state)) +
  coord_equal() +
  geom_vector(aes(head = coast)) +
  geom_text_radiate()
