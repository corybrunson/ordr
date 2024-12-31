# scaled PCA of Anderson iris data with ranges and confidence intervals
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia(1) %>%
  augment_ord() %>%
  mutate_rows(species = iris$Species) %>%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_rows_lineranges(fun.data = mean_sdl, linewidth = .75) +
  geom_rows_density_2d(contour = TRUE, alpha = .5) +
  geom_cols_vector(aes(label = name), color = "#444444", size = 3) +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris data",
    "Ranges 2 sample standard deviations from centroids"
  )
