# Align an MDS ordination to a PCA ordination
country_differences %>%
  cmdscale(k = 2) %>%
  as_tbl_ord() %>%
  print() -> differences_cmds
country_differences %>%
  prcomp() %>%
  as_tbl_ord() %>%
  print() -> differences_pca
# negate PCA cases to MDS coordinates
differences_cmds %>%
  negate_to(differences_pca, "u") %>%
  print() -> differences_align_u
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(differences_cmds) + geom_u_point(),
  ggbiplot(differences_pca) + geom_u_point(),
  ggbiplot(differences_align_u) + geom_u_point()
), ncol = 3))
# negate PCA variables to MDS coordinates
differences_cmds %>%
  negate_to(differences_pca, "v") %>%
  print() -> differences_align_v
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(differences_cmds) + geom_v_vector(),
  ggbiplot(differences_pca) + geom_v_vector(),
  ggbiplot(differences_align_v) + geom_v_vector()
), ncol = 3))
