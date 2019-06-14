# Confer PCA weights and inertia between cases and variables
country_attributes %>%
  prcomp() %>%
  as_tbl_ord() %>%
  print() -> attributes_pca
# form biplot
attributes_pca %>%
  ggbiplot(
    aes(label = .name),
    sec.axes = "v", scale.factor = dim_v(attributes_pca)
  ) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_u_text(aes(x = PC1, y = PC2), color = "seagreen") +
  geom_v_vector(aes(x = PC1, y = PC2), color = "darkred") +
  geom_v_text_repel(aes(x = PC1, y = PC2), color = "darkred")
# covariance biplot
attributes_pca %>%
  confer_inertia(0) %>%
  ggbiplot(
    aes(label = .name),
    sec.axes = "u", scale.factor = dim_u(attributes_pca)
  ) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_u_text(aes(x = PC1, y = PC2), color = "seagreen") +
  geom_v_vector(aes(x = PC1, y = PC2), color = "darkred") +
  geom_v_text_repel(aes(x = PC1, y = PC2), color = "darkred")