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
# PCA biplot with species ranges
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  # 
  geom_u_lineranges(fun.data = mean_sdl, size = .75) +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Ranges 2 sample standard deviations from centroids"
  )
# PCA biplot with species confidence intervals
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  # 99% nonparametric confidence intervals
  geom_u_lineranges(
    fun.data = mean_cl_boot, fun.args = list(conf.int = .99),
    size = .75
  ) +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "99% confidence intervals based on nonparametric bootstrap sampling"
  )
