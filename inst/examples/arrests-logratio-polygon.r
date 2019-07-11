# state abbreviations
state <- data.frame(
  .name = state.name,
  .abb = state.abb
)
# Log-ratio analysis of 1973 violent crime arrests in the United States
(arrests_lra <- lra(USArrests[, -3]))
arrests_lra %>%
  as_tbl_ord() %>%
  augment() %>%
  left_join_u(state, by = ".name") %>%
  print() -> arrests_lra
# Adapt Exhibit 7.1 in Greenacre (2010)
arrests_lra %>%
  confer_inertia(0) %>%
  ggbiplot() +
  #ggbiplot(sec.axes = "v") +
  theme_bw() +
  geom_u_text(
    aes(label = .abb),
    size = 3, color = "darkgreen", alpha = .5
  ) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(
    aes(label = .name),
    color = "brown4", fontface = "bold"
  ) +
  ggtitle(
    "Log-ratio analysis of violent crime arrest rates",
    "United States, 1973"
  ) +
  guides(color = FALSE, size = FALSE)
