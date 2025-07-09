
\dontrun{
# classical multidimensional scaling of road distances between European cities
euro_mds <- ordinate(eurodist, cmdscale_ord, k = 11)
# monoplot of city locations
euro_plot <- euro_mds %>%
  negate_ord("PCo2") %>%
  ggbiplot() +
  geom_cols_text(aes(label = name), size = 3)
print(euro_plot)
# biplot with minimal spanning tree based on plotting window distances
euro_plot +
  stat_cols_spantree(
    engine = "mlpack",
    alpha = .5, linetype = "dotted"
  )
# biplot with minimal spanning tree based on full-dimensional distances
euro_plot +
  stat_cols_spantree(
    ord_aes(euro_mds), engine = "mlpack",
    alpha = .5, linetype = "dotted"
  )
}
