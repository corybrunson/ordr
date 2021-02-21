# Principal components analysis of U.S. personal expenditure data
USPersonalExpenditure %>%
  prcomp(center = FALSE) %>%
  as_tbl_ord() %>%
  confer_inertia(c(.5, .5)) %>%
  negate_to_nonneg_orthant("v") %>%
  # allow radiating text to exceed plotting window
  ggbiplot(aes(label = .name), clip = "off") +
  geom_rows_label(size = 3) +
  geom_cols_vector() +
  # omit labels in the hull with the origin
  geom_cols_text_radiate(stat = "chull", conical = TRUE) +
  scale_x_continuous(expand = expand_scale(add = 1)) +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Symmetric biplot of un-centered PCA"
  )
