# Confer PCA weights and inertia between cases and variables
data(country_attributes)
country_attributes %>%
  prcomp() %>%
  as_tbl_ord() %>%
  print() -> attributes_pca
# form biplot
attributes_pca %>%
  ggbiplot(
    aes(label = .name),
    sec.axes = "cols", scale.factor = dim_cols(attributes_pca)
  ) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_rows_text(aes(x = PC1, y = PC2), color = "seagreen") +
  geom_cols_vector(aes(x = PC1, y = PC2), color = "darkred") +
  geom_cols_text(
    aes(x = PC1, y = PC2),
    color = "darkred", hjust = "outward", vjust = "outward"
  )
# covariance biplot
attributes_pca %>%
  confer_inertia(0) %>%
  ggbiplot(
    aes(label = .name),
    sec.axes = "rows", scale.factor = dim_rows(attributes_pca)
  ) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_rows_text(aes(x = PC1, y = PC2), color = "seagreen") +
  geom_cols_vector(aes(x = PC1, y = PC2), color = "darkred") +
  geom_cols_text_repel(aes(x = PC1, y = PC2), color = "darkred")
