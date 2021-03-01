# data frame of Anderson iris species measurements
class(iris)
head(iris)
# compute unscaled row-principal components of scaled measurements
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  print() -> iris_pca
# bind species classification to observation coordinates
(iris_pca <- mutate_rows(iris_pca, species = iris$Species))
# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
get_cols(iris_pca)
# augment measurement coordinates with names and scaling parameters
augment_ord(iris_pca)
# summarize principal components
tidy(iris_pca)
# fortification of artificial coordinates yields proportion of variance measure
fortify(iris_pca, .matrix = "coord")
# scree plot of inertia
ggplot(iris_pca, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
ggplot(iris_pca, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(iris_pca)
# row-principal biplot with coordinate-wise standard deviations
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_rows_point(alpha = .5) +
  geom_cols_vector(color = "#444444") +
  geom_cols_text_radiate(aes(label = .name), color = "#444444") +
  ggtitle("Row-principal unscaled PCA biplot of Anderson iris measurements") +
  expand_limits(y = c(NA, 2))
