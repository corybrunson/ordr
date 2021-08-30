# data frame of Anderson iris species measurements
class(iris)
head(iris)
# compute scaled row-principal components of scaled measurements
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
# summarize ordination
glance(iris_pca)
# bind species classification to observation coordinates
(iris_pca <- mutate_rows(iris_pca, species = iris$Species))
# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
get_cols(iris_pca)
# augment measurements with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
# summarize principal components
tidy(iris_pca)
# scree plot of proportion of variance (inertia)
tidy(iris_pca) %>%
  ggplot(aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_col() +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(iris_pca)
# row-principal biplot
iris_pca %>%
  augment_ord() %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_cols_axis() +
  geom_cols_axis_ticks(aes(center = .center, scale = .scale)) +
  geom_cols_axis_text(aes(center = .center, scale = .scale)) +
  geom_cols_axis_label(aes(label = .name)) +
  geom_rows_point(aes(color = species), alpha = .5) +
  ggtitle("Row-principal PCA biplot of Anderson iris measurements")
