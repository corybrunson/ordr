# Unscaled PCA and two-scaled biplot of Anderson iris data
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  mutate_rows(species = iris$Species) %>%
  mutate_cols(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_rows_point() +
  geom_cols_vector(color = "#444444") +
  geom_cols_text_radiate(aes(label = measure), color = "#444444") +
  ggtitle(
    "Row-principal unscaled PCA biplot of Anderson iris data",
    "Variable loadings scaled to secondary axes"
  ) +
  expand_limits(y = c(-1, 3.5))
