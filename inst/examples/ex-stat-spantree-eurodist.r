# classical multidimensional scaling of road distances between European cities
eurodist %>%
  cmdscale_ord(k = 11) %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> euro_mds
# biplot with minimal spanning tree based on full-dimensional distances
# (as implemented in {mlpack})
euro_mds %>%
  ggbiplot() +
  scale_y_reverse() +
  stat_cols_spantree(
    ord_aes(euro_mds), check.aes = FALSE, engine = "mlpack",
    alpha = .5, linetype = "dotted"
  ) +
  geom_cols_text(aes(label = .name), size = 3) +
  ggtitle(
    "MDS biplot of road distances between European cities",
    "Dotted segments constitute the minimal spanning tree"
  )
