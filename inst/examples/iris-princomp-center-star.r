# Scaled PCA of Anderson Iris data with centroid stars
iris[, -5] %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  mutate_u(species = iris$Species) %>%
  mutate_v(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  stat_u_star(alpha = .5, fun.center = "mean") +
  stat_u_center(size = 3, fun.center = "mean") +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Segments connect each observation to its within-species centroid"
  )
