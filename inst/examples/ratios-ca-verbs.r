# Correspondence analysis of Ruhlman's recipe ratios
ratios %>%
  select(-chapter) %>%
  tibble::column_to_rownames("recipe") %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  augment() %>%
  mutate_u(recipe = .name, chapter = ratios$chapter) %>%
  mutate_v(ingredient = .name) %>%
  print() -> ratios_ca
ratios_ca %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot() +
  theme_bw() +
  geom_v_vector(alpha = .5) +
  geom_u_text(aes(label = recipe, color = chapter)) +
  ggtitle("Row-principal CA biplot of Ruhlman recipe ratios")
