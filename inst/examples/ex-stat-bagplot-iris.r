iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
ggbiplot(iris_pca) +
  geom_rows_point(aes(color = Species)) +
  # FIXME: Outliers are computed incorrectly.
  stat_rows_bagplot(aes(color = Species, fill = Species),
                    outlier_size = 3, outlier_shape = 1L) +
  geom_cols_vector(aes(label = name))
