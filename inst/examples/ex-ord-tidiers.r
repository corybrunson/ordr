# illustrative ordination: PCA of iris data
iris_pca <- ordinate(iris, seq(4L), ~ prcomp(., center = TRUE, scale. = TRUE))
# use `tidy()` to summarize distribution of inertia
tidy(iris_pca)
# this facilitates scree plots
tidy(iris_pca) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  geom_col() +
  labs(x = "Latent dimension", y = "Inertia")
tidy(iris_pca) %>%
  ggplot(aes(x = .name, y = .prop_var)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Proportion of variance")
# use `fortify()` to prepare either matrix factor for `ggplot()`
fortify(iris_pca, .matrix = "cases")
fortify(iris_pca, .matrix = "variables")
# ... or to prepare both for `ggbiplot()`
fortify(iris_pca)
# use `glance()` to summarize the model as an ordination
glance(iris_pca)
# this enables comparisons to other models
rbind(
  glance(ordinate(subset(iris, Species == "setosa"), seq(4L), prcomp)),
  glance(ordinate(subset(iris, Species == "versicolor"), seq(4L), prcomp)),
  glance(ordinate(subset(iris, Species == "virginica"), seq(4L), prcomp))
)
