# PCA of iris data
iris_pca <- ordinate(iris, 1:4, prcomp, scale = TRUE)
# row-principal (interpolation) biplot
iris_pca %>%
  augment_ord() %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_axis(aes(label = .name, center = .center, scale = .scale)) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  ggtitle("Interpolation biplot of Anderson iris measurements")
# row-principal prediction biplot
iris_pca %>%
  augment_ord() %>%
  ggbiplot(prediction = TRUE) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_axis(aes(label = .name, center = .center, scale = .scale)) +
  geom_rows_point(aes(color = Species), alpha = .5) +
  ggtitle("Prediction biplot of Anderson iris measurements")
