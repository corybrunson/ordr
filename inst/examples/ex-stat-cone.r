mtcars$name <- rownames(mtcars)
ggplot(mtcars, aes(wt, mpg, label = name)) +
  theme_scaffold() +
  geom_text(size = 3) +
  stat_cone()
ggplot(mtcars, aes(hp, mpg, label = name)) +
  theme_scaffold() +
  geom_origin() +
  geom_text(size = 3) +
  stat_cone(origin = TRUE, linetype = "dotted")
