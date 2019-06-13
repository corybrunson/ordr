# Scaled PCA of Anderson iris data with ranges and confidence intervals
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  mutate_u(species = iris$Species) %>%
  print() -> iris_pca
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_u_lineranges(fun.data = mean_sdl, size = .75) +
  geom_u_point(alpha = .5) +
  geom_v_vector(color = "#444444") +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Ranges 2 sample standard deviations from centroids"
  )
iris_pca %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
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
