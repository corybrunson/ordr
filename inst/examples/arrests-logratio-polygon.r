# Log-ratio analysis of 1973 violent crime arrests in the United States
USArrests[, -3] %>%
  lra() %>%
  as_tbl_ord() %>%
  print() -> arrests_lra
# Adapt Exhibit 7.1 in Greenacre (2010)
arrests_lra %>%
  confer_inertia(0) %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen", size = 3, alpha = .5) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_point(color = "brown4") +
  geom_v_text(
    color = "brown4", fontface = "bold",
    hjust = "outward", vjust = "outward"
  ) +
  ggtitle(
    "Log-ratio analysis of violent crime arrest rates",
    "United States, 1973"
  )
