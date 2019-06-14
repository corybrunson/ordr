data(iris)

iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  mutate_u(species = iris$Species) %>%
  mutate_v(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca

# consistently color and fill
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  geom_u_point() +
  stat_u_center(fun.center = "mean", size = 3, shape = "triangle") +
  geom_polygon(aes(fill = species), alpha = .25, stat = "u_ellipse") +
  geom_v_vector(color = "#444444") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2)
