iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
ggbiplot(iris_pca) +
  geom_rows_point(aes(color = Species)) +
  stat_rows_bagplot(aes(color = Species, fill = Species)) +
  geom_cols_vector(aes(label = name))
