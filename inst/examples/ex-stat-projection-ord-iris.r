
# unscaled PCA
iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)
# biplot canvas
iris_biplot <- 
  iris_pca %>%
  ggbiplot(aes(color = Species, label = name), axis.type = "predictive") +
  geom_rows_point() +
  geom_cols_axis(aes(center = center))
# print select cases
top_cases <- c(1, 51, 101)
iris[top_cases, ]
# subset variables
length_vars <- c(1, 3)
iris[, length_vars] %>%
  aggregate(by = iris[, "Species", drop = FALSE], FUN = mean)

# project all cases onto all axes
iris_biplot + stat_rows_projection()
# project all cases onto select axes
iris_biplot + stat_rows_projection(ref_subset = length_vars)

# project select cases onto all axes
iris_biplot + stat_rows_projection(subset = top_cases)
# project select cases onto select axes
iris_biplot + stat_rows_projection(subset = top_cases, ref_subset = length_vars)

# project select cases onto manually provided axes
iris_cols <- as.data.frame(get_cols(iris_pca))[c(1, 2), ]
iris_biplot + stat_rows_projection(subset = top_cases, referent = iris_cols)

# project selected cases onto selected axes in full-dimensional space
iris_pca %>%
  ggbiplot(ord_aes(iris_pca, color = Species, label = name),
           axis.type = "predictive") +
  geom_rows_point() +
  geom_cols_axis(aes(center = center)) +
  stat_rows_projection(subset = top_cases, ref_subset = length_vars)
