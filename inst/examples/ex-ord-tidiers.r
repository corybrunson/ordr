# illustrative ordination: PCA of iris data
iris_pca <- ordinate(iris, ~ prcomp(., center = TRUE, scale. = TRUE), seq(4L))

# use `tidy()` to summarize distribution of inertia
tidy(iris_pca)
# this facilitates scree plots
tidy(iris_pca) %>%
  ggplot(aes(x = name, y = prop_var)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Proportion of variance")

# use `fortify()` to prepare either matrix factor for `ggplot()`
fortify(iris_pca, .matrix = "V") %>%
  ggplot(aes(x = name, y = PC1)) +
  geom_col() +
  coord_flip() +
  labs(x = "Measurement")
iris_pca %>%
  fortify(.matrix = "U") %>%
  ggplot(aes(x = PC1, fill = Species)) +
  geom_histogram() +
  labs(y = NULL)
# ... or to prepare both for `ggbiplot()`
fortify(iris_pca)

# use `glance()` to summarize the model as an ordination
glance(iris_pca)
# this enables comparisons to other models
rbind(
  glance(ordinate(subset(iris, Species == "setosa"), prcomp, seq(4L))),
  glance(ordinate(subset(iris, Species == "versicolor"), prcomp, seq(4L))),
  glance(ordinate(subset(iris, Species == "virginica"), prcomp, seq(4L)))
)
