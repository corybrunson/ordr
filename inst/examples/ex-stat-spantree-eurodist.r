\dontrun{
# classical multidimensional scaling of road distances between European cities
euro_mds <- ordinate(eurodist, cmdscale_ord, k = 11)

# biplot with minimal spanning tree based on full-dimensional distances
# (as implemented in {mlpack})
euro_mds %>%
  negate_ord("PCo2") %>%
  ggbiplot() +
  stat_cols_spantree(
    ord_aes(euro_mds), engine = "mlpack",
    alpha = .5, linetype = "dotted"
  ) +
  geom_cols_text(aes(label = name), size = 3) +
  ggtitle(
    "MDS biplot of road distances between European cities",
    "Dotted segments constitute the minimal spanning tree"
  )
}
