# compute log-ratio analysis of Freestone primary class composition measurements
glass %>%
  ordinate(cols = c(SiO2, Al2O3, CaO, FeO, MgO),
           model = lra, compositional = TRUE) %>%
  confer_inertia("rows") %>%
  print() -> glass_lra

# row-principal biplot with ordinate-wise standard deviations
glass_lra %>%
  ggbiplot(aes(color = Site), sec.axes = "cols", scale.factor = .05) +
  theme_biplot() +
  scale_color_brewer(type = "qual", palette = 6) +
  geom_cols_text(stat = "chull", aes(label = name), color = "#444444") +
  geom_rows_lineranges(fun.data = mean_sdl, linewidth = .75) +
  geom_rows_point(alpha = .5) +
  ggtitle(
    "Row-principal LRA biplot of Freestone glass measurements",
    "Ranges 2 sample standard deviations from centroids"
  )
