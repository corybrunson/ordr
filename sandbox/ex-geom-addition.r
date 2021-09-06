iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
iris_pca <- mutate_rows(iris_pca, species = iris$Species)
iris_pca <- augment_ord(iris_pca)
new_data <- iris[c(42, 61, 110), , drop = FALSE]
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = .name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "circle", alpha = .5) +
  geom_cols_addition(aes(center = .center, scale = .scale, linetype = measure),
                     new_data = new_data, type = "sequence", alpha = .25) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = .name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "circle", alpha = .5) +
  geom_cols_addition(aes(center = .center, scale = .scale),
                     new_data = new_data, type = "centroid", alpha = .25) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)
