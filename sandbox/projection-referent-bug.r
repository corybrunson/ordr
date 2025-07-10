iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)
top_cases <- c(1, 51, 101)
iris_cols <- as.data.frame(get_cols(iris_pca))[c(1, 2), , drop = FALSE]
iris_biplot <- 
  iris_pca %>%
  ggbiplot(aes(color = Species, label = name), axis.type = "predictive") +
  geom_rows_point() +
  geom_cols_axis(aes(center = center)) +
  stat_rows_projection(subset = top_cases, referent = iris_cols)
iris_biplot
