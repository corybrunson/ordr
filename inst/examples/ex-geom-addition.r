# TODO: write generics (or functions of recovered components?) to calculate
# vector components of these constructions, per issue #4

# calculate predictions
iris_pca <- prcomp(iris[, -5], scale = TRUE)
new_data <- iris[c(42, 61, 110), -5, drop = FALSE]
mat_data <- as.matrix(new_data[, rownames(iris_pca$rotation)])
std_data <- 
  sweep(sweep(mat_data, 2, iris_pca$center, "-"), 2, iris_pca$scale, "/")
pred <- std_data %*% iris_pca$rotation[, seq(2)]
# calculate components?


iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
iris_pca <- mutate_rows(iris_pca, species = iris$Species)
iris_pca <- augment_ord(iris_pca)
new_data <- iris[c(42, 61, 110), , drop = FALSE]
# centroid interpolation method
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "cross") +
  geom_cols_addition(
    aes(center = center, scale = scale, interpolate = name),
    new_data = new_data, type = "centroid"
  ) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)
# artificial missingness
new_data[2L, "Sepal.Length"] <- NA
new_data[3L, "Sepal.Width"] <- NA
new_data[1L, "Petal.Length"] <- NA
new_data$Petal.Width <- NULL
# sequence interpolation method
iris_pca %>%
  augment_ord() %>%
  mutate_rows(obs = dplyr::row_number()) %>%
  mutate_cols(measure = name) %>%
  ggbiplot() +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  geom_origin(marker = "circle") +
  geom_cols_addition(
    aes(center = center, scale = scale, interpolate = name,
        linetype = measure),
    new_data = new_data, type = "sequence"
  ) +
  geom_rows_text(aes(label = obs, color = species), alpha = .5, size = 3)
