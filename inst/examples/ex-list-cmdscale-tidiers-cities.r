# 'dist' object (matrix of road distances) of large American cities
class(UScitiesD)
print(UScitiesD)

# use multidimensional scaling to infer artificial planar coordinates
UScitiesD %>%
  cmdscale(k = 3L, eig = TRUE, x.ret = TRUE) ->
  usa_mds

# glance at the model
glance(usa_mds)
# return the tidied coordinates
tidy(usa_mds)
# return the upper triangle of the doubly-centered distance matrix in tidy form
tidy(usa_mds, matrix = "x")
# return the eigenvalues for the principal coordinates, with summary statistics
tidy(usa_mds, matrix = "eig")
# reorient to conventional compass
tidy(usa_mds) %>%
  ggplot(aes(x = -PCo1, y = -PCo2)) +
  geom_text(aes(label = point), size = 3) +
  ggtitle("MDS biplot of distances between U.S. cities")
