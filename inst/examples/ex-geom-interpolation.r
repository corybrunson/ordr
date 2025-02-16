iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
iris_pca <- mutate_rows(iris_pca, species = iris$Species)
iris_pca <- augment_ord(iris_pca)

# sample of one of each species, with some missing measurements
new_data <- iris[c(42, 61, 110), seq(5, 1), drop = FALSE]
new_data[3L, "Sepal.Width"] <- NA
new_data[1L, "Petal.Length"] <- NA
print(new_data)

# centroid interpolation method
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "cross", alpha = .5) +
  geom_cols_interpolation(
    aes(center = center, scale = scale, interpolate = name), size = 3,
    new_data = new_data, type = "centroid", alpha = .5
  ) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)

# missing an entire variable
new_data$Petal.Length <- NULL

# sequence interpolation method
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "circle", alpha = .5) +
  geom_cols_interpolation(
    aes(center = center, scale = scale, interpolate = name,
        linetype = measure),
    new_data = new_data, type = "sequence", alpha = .5
  ) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)
