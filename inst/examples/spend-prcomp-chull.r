# Principal components analysis of U.S. personal expenditure data
USPersonalExpenditure %>%
  prcomp(center = FALSE) %>%
  as_tbl_ord() %>%
  confer_inertia(c(.5, .5)) %>%
  negate_to_nonneg_orthant("v") %>%
  ggbiplot(aes(label = .name)) +
  geom_u_label(size = 3) +
  geom_v_vector() +
  # omit labels in the hull with the origin
  geom_v_text_radiate(stat = "chull", conical = TRUE) +
  scale_x_continuous(expand = expand_scale(add = 1)) +
  scale_y_continuous(expand = expand_scale(add = .5)) +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Symmetric biplot of un-centered PCA"
  )
