iris_pca <- prcomp(iris[, seq(4)], rank. = 1)
iris_rows <- 
  iris_pca$x |> as.data.frame() |> 
  tibble::rownames_to_column(var = "id") |> 
  transform(species = iris$Species)
iris_cols <- 
  iris_pca$rotation |> as.data.frame() |> 
  tibble::rownames_to_column(var = "measurement")
ggplot(iris_rows, aes(x = PC1)) +
  geom_density(aes(color = species)) +
  geom_pin(
    aes(x = PC1 * 10, label = measurement), ymax = 1,
    data = iris_cols,
    check_overlap = TRUE
  ) +
  scale_x_continuous(sec.axis = ~ . / 10)
