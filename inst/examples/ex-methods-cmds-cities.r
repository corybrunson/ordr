# 'dist' object (matrix of road distances) of large American cities
class(UScitiesD)
print(UScitiesD)

# use multidimensional scaling to infer artificial planar coordinates
UScitiesD %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  print() -> usa_mds

# recover (equivalent) matrices of row and column artificial coordinates
get_rows(usa_mds)
get_cols(usa_mds)

# augment ordination with point names
(usa_mds <- augment_ord(usa_mds))

# reorient biplot to conventional compass
usa_mds %>%
  negate_ord(c(1, 2)) %>%
  ggbiplot() +
  geom_cols_text(aes(label = name), size = 3) +
  ggtitle("MDS biplot of distances between U.S. cities")
