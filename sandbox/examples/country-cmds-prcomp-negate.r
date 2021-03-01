# Compare an MDS to a PCA
data(country_differences)
country_differences %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  print() -> differences_cmds
country_differences %>%
  prcomp() %>%
  as_tbl_ord() %>%
  print() -> differences_pca
# negate PCA axis to match MDS axis
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(differences_cmds) + geom_rows_point(),
  ggbiplot(differences_pca) + scale_y_reverse() + geom_rows_point()
), ncol = 2))
