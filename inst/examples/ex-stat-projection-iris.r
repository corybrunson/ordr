# basic PCA
iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)

# basic biplot
iris_biplot <- 
  ggbiplot(iris_pca, aes(color = Species, label = name)) +
  geom_rows_point() +
  geom_cols_axis()
# project all cases onto all axes
iris_biplot + stat_rows_projection()
# project all cases onto select axes
iris_biplot + stat_rows_projection(referent = c(2, 4))
# project select cases onto all axes
iris_biplot + stat_rows_projection(subset = c(1, 51, 101))
# project select cases onto select axes
iris_biplot + stat_rows_projection(subset = c(1, 51, 101), referent = c(2, 4))
# project select cases onto manually provided axes
iris_cols <- as.data.frame(get_cols(iris_pca))
iris_biplot + stat_rows_projection(subset = c(1, 51, 101), referent = iris_cols)

# project selected cases onto selected axes in full-dimensional space
ggbiplot(iris_pca, ord_aes(iris_pca, color = Species, label = name)) +
  geom_rows_point() +
  geom_cols_axis() +
  stat_rows_projection(subset = c(1, 51, 101), referent = c(2, 4))
