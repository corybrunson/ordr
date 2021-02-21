# Flip MDS for distances between American cities with compass orientation
UScitiesD %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> usa_mds
usa_mds %>%
  ggbiplot() +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_cols_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of distances between American cities")
