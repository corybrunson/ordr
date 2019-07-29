# Regression analysis of Motor Trend design and performance data
mtcars %>%
  scale(scale = FALSE) %>%
  as.data.frame() %>%
  lm(formula = mpg ~ wt + cyl) %>%
  as_tbl_ord() %>%
  augment() %>%
  mutate_u(influence = .wt.res^2) %>%
  print() -> mtcars_lm
mtcars_lm %>%
  ggbiplot(aes(x = wt, y = cyl)) +
  geom_u_point(aes(color = influence)) +
  geom_v_vector() +
  # weight isolines
  geom_v_isolines(axes = 1, by = 5) +
  ggtitle(
    "Weight isolines with data colored by importance",
    "Regressing mpg onto weight and number of cylinders"
  )
