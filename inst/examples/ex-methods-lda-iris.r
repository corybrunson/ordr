# data frame of Anderson iris species measurements
class(iris)
head(iris)
# default (unstandardized discriminant) coefficients
lda_ord(iris[, 1:4], iris[, 5]) %>%
  as_tbl_ord() %>%
  print() -> iris_lda
# summarize ordination
glance(iris_lda)
# recover centroid coordinates and measurement discriminant coefficients
get_rows(iris_lda, .supplement = FALSE)
get_cols(iris_lda)
# augment ordination with centroid and measurement names
augment_ord(iris_lda)
# summarize linear discriminant axes
tidy(iris_lda)
# scree plot of inertia
ggplot(tidy(iris_lda), aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_col() +
  labs(x = "", y = "Proportion of inertia")
# fortification (without supplement)
fortify(iris_lda, .supplement = FALSE)
# unstandardized coefficient LDA biplot (centroids only)
iris_lda %>%
  augment_ord() %>%
  mutate_rows(species = .grouping) %>%
  ggbiplot(.supplement = FALSE) +
  theme_bw() +
  geom_rows_point(aes(color = .grouping), size = 3) +
  geom_cols_vector(color = "#888888") +
  geom_cols_text_radiate(aes(label = .name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle(
    "Row-standardized biplot of iris LDA",
    "Unstandardized measurement coefficients and species centroids"
  ) +
  expand_limits(y = c(-2, 4.25))
# biplot with supplementary points for observations
iris_lda %>%
  augment_ord() %>%
  mutate_rows(
    species = .grouping,
    discriminant = ifelse(! .supplement, "centroid", "case")
  ) %>%
  ggbiplot() +
  theme_bw() +
  geom_rows_point(aes(
    color = .grouping,
    size = discriminant, alpha = discriminant
  )) +
  geom_cols_vector(color = "#888888") +
  geom_cols_text_radiate(aes(label = .name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle(
    "Row-standardized biplot of iris LDA",
    "Unstandardized measurement coefficients, observations, & species centroids"
  ) +
  expand_limits(y = c(-3, 5))
