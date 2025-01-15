iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
# FIXME: Generated shortcuts don't preserve internal processes.
ggbiplot(iris_pca) +
  stat_rows_bagplot(aes(fill = Species), fence.linewidth = 0.25) +
  geom_cols_vector(aes(label = name))

\dontrun{
  # FIXME: Generated shortcuts don't preserve internal processes.
  ggbiplot(iris_pca) +
  stat_rows_bagplot(aes(fill = Species), median.colour = sync()) +
  geom_cols_vector(aes(label = name))
}
