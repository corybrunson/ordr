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
(usa_mds <- augment_ord(usa_mds))
# summarize artifical coordinates
tidy(usa_mds)
# scree plot of inertia
ggplot(tidy(usa_mds), aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# reorient biplot to conventional compass
usa_mds %>%
  ggbiplot() +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_cols_text(aes(label = .name), size = 3) +
  ggtitle("MDS biplot of distances between U.S. cities")
