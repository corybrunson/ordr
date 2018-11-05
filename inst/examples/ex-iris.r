data(iris)

iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  mutate_u(species = iris$Species) %>%
  mutate_v(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca

# default confidence ellipses
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  stat_u_ellipse() +
  geom_v_vector(color = "#222222")

# consistently color and fill
# NOTE: 'color' and 'fill' don't match
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  geom_polygon(aes(fill = species), alpha = .25, stat = "u_ellipse") +
  geom_v_vector(color = "#222222")

# symmetric biplot
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  stat_u_ellipse() +
  geom_v_vector(color = "#222222") +
  geom_v_text_repel(aes(label = measure), color = "#222222")
