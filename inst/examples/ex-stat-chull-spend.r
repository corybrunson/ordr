# centered principal components analysis of U.S. personal expenditure data
USPersonalExpenditure %>%
  prcomp() %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  # allow radiating text to exceed plotting window
  ggbiplot(aes(label = name), clip = "off",
           sec.axes = "cols", scale.factor = 50) +
  geom_rows_label(size = 3) +
  geom_cols_vector() +
  # omit labels in the conical hull without the origin
  stat_cols_cone(linetype = "dotted") +
  geom_cols_text_radiate(stat = "cone") +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Row-principal biplot of centered PCA"
  )
