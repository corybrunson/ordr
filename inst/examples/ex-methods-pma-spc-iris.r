# data frame of Anderson iris species measurements
class(iris)
head(iris)
# compute principal components of scaled measurements
iris_spca <- PMA::SPC(
  as.matrix(iris[, -5L]),
  sumabsv = sqrt(2),
  K = 2L,
  cnames = names(iris)[seq(4L)]
)
# wrap as a 'tbl_ord' object
(iris_spca <- as_tbl_ord(iris_spca))
# summarize ordination
glance(iris_spca)
# bind species classification to observation coordinates
(iris_spca <- mutate_rows(iris_spca, species = iris$Species))
# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_spca))
get_cols(iris_spca)
# augment measurements with names and scaling parameters
(iris_spca <- augment_ord(iris_spca))
# summarize principal components
tidy(iris_spca)
# scree plot of proportion of variance (inertia)
tidy(iris_spca) %>%
  ggplot(aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_col() +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(iris_spca)
# row-principal biplot
iris_spca %>%
  augment_ord() %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  # -+- text elements are extremely far removed -+-
  geom_cols_axis(aes(label = .name), center = iris_spca$meanx) +
  geom_rows_point(aes(color = species), alpha = .5) +
  ggtitle("Row-principal sparse PCA biplot of Anderson iris measurements")
