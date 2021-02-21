# Align an MDS ordination to a PCA ordination
data(country_differences)
country_differences %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  print() -> differences_cmds
country_differences %>%
  prcomp() %>%
  as_tbl_ord() %>%
  print() -> differences_pca
# negate PCA cases to MDS coordinates
differences_cmds %>%
  negate_to(differences_pca, "rows") %>%
  print() -> differences_align_rows
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(differences_cmds) + geom_rows_point(),
  ggbiplot(differences_pca) + geom_rows_point(),
  ggbiplot(differences_align_rows) + geom_rows_point()
), ncol = 3))
# negate PCA variables to MDS coordinates
differences_cmds %>%
  negate_to(differences_pca, "cols") %>%
  print() -> differences_align_cols
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(differences_cmds) + geom_cols_vector(),
  ggbiplot(differences_pca) + geom_cols_vector(),
  ggbiplot(differences_align_cols) + geom_cols_vector()
), ncol = 3))
