# Compare MDS to the spanning tree for road distances between European cities
eurodist %>%
  cmdscale(k = 11) %>%
  as_tbl_ord() %>%
  augment() %>%
  # align to conventional compass orientation
  negate(2) %>%
  print() -> city_mds
city_mds %>%
  ggbiplot() +
  stat_v_spantree(
    ord_aes(city_mds), check.aes = FALSE,
    alpha = .5, linetype = "dotted"
  ) +
  geom_v_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of road distances between European cities")
