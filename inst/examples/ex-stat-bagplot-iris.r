iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
ggbiplot(iris_pca) +
  stat_rows_bagplot(aes(fill = Species), fence_linewidth = 0.25) +
  geom_cols_vector(aes(label = name))

\dontrun{
# FIXME: Component-specific aesthetics should be able to vary with variables.
# FIXME: Try manipulating columns in `$setup_data()`.
ggbiplot(iris_pca) +
  stat_rows_bagplot(aes(fill = Species, median_colour = Species)) +
  geom_cols_vector(aes(label = name))
}
