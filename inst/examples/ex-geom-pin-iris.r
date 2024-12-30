iris_pca <- 
  ordinate(iris, cols = 1:4, model = ~ prcomp(., scale. = TRUE, rank. = 1))
# TODO: Only provide secondary axes for model coordinates (here, `x`).
ggbiplot(iris_pca, sec.axes = "cols", scale.factor = 10) +
  geom_rows_density(aes(color = Species)) +
  geom_cols_pin(aes(label = name), ymax = 1, check_overlap = TRUE)
