# Align MDS for distances between American cities with compass orientation
UScitiesD %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> usa_mds
usa_mds %>%
  ggbiplot() +
  geom_v_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of distances between American cities")
get_alignment(usa_mds)
# negation
usa_mds %>%
  negate_ord(1:2) %>%
  print() -> usa_mds_negate
get_alignment(usa_mds_negate)
usa_mds_negate %>%
  ggbiplot() +
  geom_v_text(aes(label = .name), size = 3) +
  ggtitle("Negated MDS biplot of distances between American cities")
# rotation
usa_mds %>%
  rotate_ord(-diag(2)) %>%
  print() -> usa_mds_rotate
get_alignment(usa_mds_rotate)
usa_mds_rotate %>%
  ggbiplot() +
  geom_v_text(aes(label = .name), size = 3) +
  ggtitle("Rotated MDS biplot of distances between American cities")
