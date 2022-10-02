# principal components analysis of overt & chemical diabetes test values
heplots::Diabetes[, seq(5L)] %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  cbind_rows(group = heplots::Diabetes$group) %>%
  augment_ord() %>%
  print() -> diabetes_pca

# note that column standard coordinates are unit vectors
rowSums(get_cols(diabetes_pca)^2)

# plot column standard coordinates with a unit circle underlaid
diabetes_pca %>%
  ggbiplot(aes(label = name), sec.axes = "cols", scale.factor = 3) +
  geom_rows_point(aes(color = group), alpha = .25) +
  geom_unit_circle(alpha = .5, scale.factor = 3) +
  geom_cols_vector() +
  geom_cols_text_radiate()
