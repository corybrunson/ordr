# Correspondence analysis of Ruhlman's recipe ratios
data(ratios)
ratios %>%
  dplyr::select(-chapter) %>%
  tibble::column_to_rownames("recipe") %>%
  dplyr::select(-chocolate) %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  augment() %>%
  mutate_rows(recipe = .name, chapter = ratios$chapter) %>%
  mutate_cols(ingredient = .name) %>%
  print() -> ratios_ca
ratios_ca %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot() +
  theme_bw() +
  geom_cols_vector(alpha = .5) +
  stat_cols_chull(geom = "text_radiate", aes(label = ingredient), alpha = .5) +
  geom_rows_text(aes(label = recipe, color = chapter), size = 4) +
  ggtitle("Row-principal CA biplot of Ruhlman recipe ratios")
