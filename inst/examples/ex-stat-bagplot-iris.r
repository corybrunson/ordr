iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
ggbiplot(iris_pca) +
  geom_rows_point(aes(color = Species)) +
  # FIXME: Component aesthetics should be handled as in `geom_boxplot()`.
  stat_rows_bagplot(aes(color = Species, fill = Species),
                    fence_linetype = 1, fence_color = "black",
                    outlier_size = 3, outlier_shape = 1L) +
  geom_cols_vector(aes(label = name))
