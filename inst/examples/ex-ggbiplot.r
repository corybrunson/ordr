# Anderson iris sepal and petal data
head(iris)
# PCA ordination
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  # row-principal conference of intertia
  confer_inertia(1) %>%
  mutate_u(species = iris$Species) %>%
  mutate_v(measure = gsub("\\.", " ", tolower(names(iris)[-5]))) %>%
  print() -> iris_pca
# PCA biplot
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point(alpha = .5) +
  # species centroids
  stat_u_center(fun.center = "mean", size = 3, shape = "triangle") +
  stat_u_ellipse() +
  geom_v_vector(color = "#444444") +
  ggtitle("Row-principal PCA biplot of Anderson iris data")
# rescaled PCA biplot
iris_pca %>%
  ggbiplot(aes(color = species), sec.axes = "v", scale.factor = 3) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_point() +
  geom_v_vector(color = "#444444") +
  geom_v_text_radiate(aes(label = measure), color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Variable loadings scaled to secondary axes"
  )

# Motor Trend automobile design and performance data
head(mtcars)
# linear regression ordination
mtcars %>%
  scale(scale = FALSE) %>%
  as.data.frame() %>%
  lm(formula = mpg ~ wt + cyl) %>%
  as_tbl_ord() %>%
  augment() %>%
  mutate_u(influence = .wt.res^2) %>%
  print() -> mtcars_lm
# linear regression biplot
mtcars_lm %>%
  ggbiplot(aes(x = wt, y = cyl)) +
  geom_u_point(aes(color = influence)) +
  geom_v_vector() +
  # weight isolines
  geom_v_isolines(ids = 1, by = 5) +
  ggtitle(
    "Weight isolines with data colored by importance",
    "Regressing mpg onto weight and number of cylinders"
  )
