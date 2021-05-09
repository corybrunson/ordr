# 'dist' object (matrix of road distances) of large American cities
class(UScitiesD)
print(UScitiesD)
# use multidimensional scaling to infer artificial planar coordinates
UScitiesD %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  print() -> usa_mds
# summarize ordination
glance(usa_mds)
# recover (equivalent) matrices of row and column artificial coordinates
get_rows(usa_mds)
get_cols(usa_mds)
# augment ordination with point names
augment_ord(usa_mds)
# summarize artifical coordinates
tidy(usa_mds)
# fortification of artificial coordinates yields proportion of variance
fortify(usa_mds, .matrix = "coord")
# scree plot of inertia
ggplot(usa_mds, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# fortification automatically augments artificial coordinates
fortify(usa_mds)
# reorient biplot to conventional compass
usa_mds %>%
  ggbiplot() +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_cols_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of distances between U.S. cities")
