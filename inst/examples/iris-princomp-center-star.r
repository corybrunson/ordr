# Scaled PCA of Anderson Iris data with centroid stars
iris[, -5] %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  mutate_rows(species = iris$Species) %>%
  mutate_cols(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  stat_rows_star(alpha = .5, fun.center = "mean") +
  stat_rows_center(size = 3, fun.center = "mean") +
  geom_rows_point(alpha = .5) +
  geom_cols_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Segments connect each observation to its within-species centroid"
  )
