
# centered principal components analysis of U.S. personal expenditure data
USPersonalExpenditure %>%
  prcomp() %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  # allow radiating text to exceed plotting window
  ggbiplot(aes(label = name), clip = "off",
           sec.axes = "cols", scale.factor = 50) +
  geom_rows_label(size = 3) +
  # omit labels in the conical hull without the origin
  geom_cols_vector(vector_labels = FALSE) +
  stat_cols_cone(linetype = "dotted") +
  geom_cols_vector(stat = "cone", vector_labels = TRUE, color = "transparent") +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Row-principal biplot of centered PCA"
  )
