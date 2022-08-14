# data frame of violent crime arrests in the United States
class(USArrests)
head(USArrests)
# get state abbreviation data
state <- data.frame(
  name = state.name,
  abb = state.abb
)

# compute (non-compositional, unweighted) log-ratio analysis
USArrests %>%
  subset(select = -UrbanPop) %>%
  lra() %>%
  as_tbl_ord() %>%
  print() -> arrests_lra

# augment log-ratio profiles with names and join state abbreviations
arrests_lra %>%
  augment_ord() %>%
  left_join_rows(state, by = "name") %>%
  print() -> arrests_lra

# recover state and arrest profiles
head(get_rows(arrests_lra))
get_cols(arrests_lra)
# initially, inertia is conferred on neither factor
get_conference(arrests_lra)

# row-principal biplot
arrests_lra %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(color = .matrix), sec.axes = "cols", scale.factor = 1/20) +
  scale_color_manual(values = c("tomato4", "turquoise4")) +
  theme_bw() +
  geom_rows_text(aes(label = abb), size = 3, alpha = .75) +
  geom_cols_polygon(fill = NA, linetype = "dashed") +
  geom_cols_text(aes(label = name, size = weight), fontface = "bold") +
  scale_size_area(guide = "none") +
  ggtitle(
    "Non-compositional LRA of violent crime arrest rates",
    "United States, 1973"
  ) +
  expand_limits(x = c(-.35)) +
  guides(color = "none")
