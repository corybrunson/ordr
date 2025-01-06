# unscaled PCA
iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)

# biplot canvas
iris_biplot <- 
  iris_pca |> 
  ggbiplot(aes(color = Species, label = name), axis.type = "predictive") +
  geom_rows_point() +
  geom_cols_axis(aes(center = center))

# project all cases onto all axes
iris_biplot + stat_rows_projection()
# project all cases onto select axes
iris_biplot + stat_rows_projection(ref_subset = c(2, 4))

# print select cases
top_cases <- c(1, 51, 101)
iris[top_cases, ]
# project select cases onto all axes
iris_biplot + stat_rows_projection(subset = top_cases)
# project select cases onto select axes
iris_biplot + stat_rows_projection(subset = top_cases, ref_subset = c(2, 4))

# project select cases onto manually provided axes
iris_cols <- as.data.frame(get_cols(iris_pca))
iris_biplot + stat_rows_projection(subset = top_cases, referent = iris_cols)

# project selected cases onto selected axes in full-dimensional space
ggbiplot(iris_pca, ord_aes(iris_pca, color = Species, label = name)) +
  geom_rows_point() +
  geom_cols_axis() +
  stat_rows_projection(subset = top_cases, ref_subset = c(2, 4))
