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
  stat_u_center(size = 3, shape = "triangle") +
  stat_u_ellipse() +
  geom_v_vector(color = "#444444")

# consistently color and fill
# NOTE: 'color' and 'fill' don't match
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  geom_polygon(aes(fill = species), alpha = .25, stat = "u_ellipse") +
  geom_v_vector(color = "#444444")

# symmetric biplot
set.seed(58319)
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  stat_u_center(size = 3, shape = "triangle") +
  stat_u_ellipse() +
  geom_v_vector(color = "#444444") +
  geom_v_text_repel(aes(label = measure), color = "#444444")

# species centers and ranges
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_lineranges(fun.data = mean_cl_boot) +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444")

# species stars
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  stat_u_star(alpha = .5, fun.center = "mean") +
  stat_u_center(size = 3, fun.center = "mean") +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444")
