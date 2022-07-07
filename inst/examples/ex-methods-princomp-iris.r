# data frame of Anderson iris species measurements
class(iris)
head(iris)
# compute unscaled row-principal components of scaled measurements
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  print() -> iris_pca
# summarize ordination
glance(iris_pca)
# bind species classification to observation coordinates
(iris_pca <- mutate_rows(iris_pca, species = iris$Species))
# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
get_cols(iris_pca)
# augment measurement coordinates with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
# summarize principal components
tidy(iris_pca)
# scree plot of inertia
tidy(iris_pca) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# fortification adds all above columns
fortify(iris_pca)
# row-principal (interpolation) biplot with coordinate-wise standard deviations
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_unit_circle() +
  geom_rows_point(alpha = .5) +
  geom_cols_vector(color = "#444444") +
  geom_cols_text_radiate(aes(label = .name), color = "#444444") +
  ggtitle("Row-principal unscaled PCA biplot of Anderson iris measurements") +
  expand_limits(y = c(NA, 2))
# row-principal prediction biplot
iris_pca %>%
  ggbiplot(prediction = TRUE, aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_unit_circle() +
  geom_rows_point(alpha = .5) +
  geom_cols_axis(aes(label = .name, center = .center, scale = .scale),
                 color = "#444444") +
  ggtitle("Row-principal unscaled PCA biplot of Anderson iris measurements") +
  expand_limits(y = c(NA, 2))
