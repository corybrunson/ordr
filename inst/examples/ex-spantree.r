# minimum spanning tree examples

# PCA of iris data set
iris[, 1:4] %>%
  prcomp(center = TRUE, scale = FALSE) %>%
  as_tbl_ord() %>%
  augment() %>%
  bind_cols_u(species = iris$Species) %>%
  confer_inertia("rows") %>%
  print() -> iris_pca

# biplot with a single MST based on the first two PCs
ggbiplot(iris_pca) +
  geom_v_vector(alpha = .5) +
  stat_u_spantree(alpha = .5) +
  geom_u_point(aes(color = species))

# biplot with species-specific MSTs based on the first two PCs
ggbiplot(iris_pca) +
  geom_v_vector(alpha = .5) +
  stat_u_spantree(aes(color = species), alpha = .5) +
  geom_u_point(aes(color = species))

# biplot with a single MST based on distances in full coordinates
ggbiplot(iris_pca) +
  geom_v_vector(alpha = .5) +
  stat_u_spantree(ord_aes(iris_pca), alpha = .5, check.aes = FALSE) +
  geom_u_point(aes(color = species))

# biplot with species-specific MSTs based on distances in full coordinates
ggbiplot(iris_pca) +
  geom_v_vector(alpha = .5) +
  stat_u_spantree(
    ord_aes(iris_pca, color = species), alpha = .5,
    check.aes = FALSE
  ) +
  geom_u_point(aes(color = species))

# biplot with species-specific Manhattan MSTs based on full coordinates
ggbiplot(iris_pca) +
  geom_v_vector(alpha = .5) +
  stat_u_spantree(
    ord_aes(iris_pca, color = species), alpha = .5,
    method = "manhattan", check.aes = FALSE
  ) +
  geom_u_point(aes(color = species))
