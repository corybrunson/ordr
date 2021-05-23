# Log-ratio analysis of shared ingredients in Ruhlman's recipe ratios
data(ratios)
ratios %>%
  dplyr::select(chapter, recipe, flour, egg, butter) %>%
  dplyr::filter(flour > 0 & egg > 0 & butter > 0) %>%
  dplyr::group_by(flour, egg, butter) %>%
  dplyr::summarize(chapter = unique(chapter),
                   recipes = stringr::str_c(recipe, collapse = " or ")) %>%
  dplyr::ungroup() %>%
  print() -> sub_ratios
sub_ratios %>%
  dplyr::select(-chapter, -recipes) %>%
  lra(compositional = TRUE) %>%
  as_tbl_ord() %>%
  augment() %>%
  cbind_rows(dplyr::select(sub_ratios, chapter, recipes)) %>%
  print() -> lra_ratios
lra_ratios %>%
  confer_inertia("rows") %>%
  ggbiplot(sec.axes = "cols", scale.factor = .05) +
  geom_rows_text(aes(label = recipes, color = chapter)) +
  geom_cols_vector() +
  geom_cols_text(aes(label = .name), hjust = "outward", vjust = "outward") +
  expand_limits(x = c(-.2, .2)) +
  ggtitle("Log-ratio biplot of flour-butter-egg ratios in baking recipes") +
  expand_limits(x = c(-.35, .35))
