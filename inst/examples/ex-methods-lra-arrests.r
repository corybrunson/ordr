# data frame of violent crime arrests in the United States
class(USArrests)
head(USArrests)
# get state abbreviation data
state <- data.frame(
  .name = state.name,
  .abb = state.abb
)
# compute (non-compositional, unweighted) log-ratio analysis
USArrests %>%
  subset(select = -UrbanPop) %>%
  lra() %>%
  as_tbl_ord() %>%
  print() -> arrests_lra
# augment log-ratio profiles with names and join state abbreviations
arrests_lra %>%
  augment() %>%
  left_join_rows(state, by = ".name") %>%
  print() -> arrests_lra
# recover state and arrest profiles
head(get_rows(arrests_lra))
get_cols(arrests_lra)
# summarize principal components
tidy(arrests_lra)
# fortification of artificial coordinates yields proportion of variance measure
fortify(arrests_lra, .matrix = "coord")
# scree plot of inertia
ggplot(arrests_lra, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
ggplot(arrests_lra, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(arrests_lra)
# initially, inertia is conferred on neither factor
get_conference(arrests_lra)
# row-principal biplot with coordinate-wise standard deviations
arrests_lra %>%
  confer_inertia(1) %>%
  ggbiplot(aes(color = .matrix), sec.axes = "cols", scale.factor = 1/20) +
  scale_color_manual(values = c("brown4", "darkgreen")) +
  theme_bw() +
  geom_rows_text(aes(label = .abb), size = 3, alpha = 2/3) +
  geom_cols_polygon(fill = NA, linetype = "dashed") +
  geom_cols_text(aes(label = .name), fontface = "bold") +
  ggtitle(
    "Log-ratio analysis of violent crime arrest rates",
    "United States, 1973"
  ) +
  guides(color = FALSE, size = FALSE)
