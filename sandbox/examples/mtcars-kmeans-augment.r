# Interpreting (k-means) clusters as ordinations on the Motor Trends data
mtcars %>%
  scale() %>%
  kmeans(centers = 3) %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> mtcars_kmeans
mtcars_kmeans %>%
  tidy() %>%
  transform(.sdev = sqrt(.withinss / .size)) %>%
  print() -> mtcars_coord
# discriminate between clusters 1 and 2
mtcars_kmeans %>%
  ggbiplot(color = factor(.cluster)) +
  geom_jitter(stat = "rows", aes(shape = .cluster), width = .2, height = .2) +
  geom_cols_vector(aes(color = `3`)) +
  scale_color_distiller(type = "div", limits = c(-1, 1)) +
  geom_cols_text_radiate(aes(label = .name)) +
  ggtitle(
    "Performance and design variable loadings onto clusters 1 and 2",
    "Color indicates loadings onto cluster 3"
  )
