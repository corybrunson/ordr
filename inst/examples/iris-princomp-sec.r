# Unscaled PCA and two-scaled biplot of Anderson iris data
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  mutate_u(species = iris$Species) %>%
  mutate_v(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  geom_v_vector(color = "#444444") +
  geom_v_text_radiate(aes(label = measure), color = "#444444") +
  ggtitle(
    "Row-principal unscaled PCA biplot of Anderson iris data",
    "Variable loadings scaled to secondary axes"
  )
