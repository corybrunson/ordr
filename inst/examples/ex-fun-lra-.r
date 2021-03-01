# U.S. 1973 violent crime arrests
head(USArrests)
# row and column subsets
state_examples <- c("Hawaii", "Mississippi", "North Dakota")
arrests <- c(1L, 2L, 4L)
# pairwise log-ratios of violent crime arrests for two states
arrest_pairs <- combn(arrests, 2L)
arrest_ratios <-
  USArrests[, arrest_pairs[1L, ]] / USArrests[, arrest_pairs[2L, ]]
colnames(arrest_ratios) <- paste(
  colnames(USArrests)[arrest_pairs[1L, ]], "/",
  colnames(USArrests)[arrest_pairs[2L, ]], sep = ""
)
arrest_logratios <- log(arrest_ratios)
arrest_logratios[state_examples, ]
# non-compositional log-ratio analysis
(arrests_lra <- lra(USArrests[, c(1, 2, 4)]))
(arrests_lra_ord <- augment(as_tbl_ord(arrests_lra)))
# row-principal biplot
arrests_lra_ord %>%
  confer_inertia("rows") %>%
  ggbiplot(
    aes(label = .name, color = .matrix),
    sec.axes = "cols", scale.factor = 1/20
  ) +
  scale_color_manual(values = c("tomato4", "turquoise4")) +
  theme_bw() +
  geom_rows_text(size = 3, alpha = .5) +
  geom_cols_polygon(fill = NA, linetype = "dashed") +
  geom_cols_text(fontface = "bold") +
  ggtitle(
    "Non-compositional LRA of violent crime arrest rates",
    "United States, 1973"
  ) +
  expand_limits(x = c(-.35)) +
  guides(color = FALSE)
# compositional log-ratio analysis
(arrests_lra <- lra(USArrests[, c(1, 2, 4)], compositional = TRUE))
(arrests_lra_ord <- augment(as_tbl_ord(arrests_lra)))
# row-principal biplot
arrests_lra_ord %>%
  confer_inertia("rows") %>%
  ggbiplot(
    aes(label = .name, color = .matrix),
    sec.axes = "cols", scale.factor = 1/20
  ) +
  scale_color_manual(values = c("tomato4", "turquoise4")) +
  theme_bw() +
  geom_rows_text(size = 3, alpha = .5) +
  geom_cols_polygon(fill = NA, linetype = "dashed") +
  geom_cols_text(fontface = "bold") +
  ggtitle(
    "Compositional LRA of violent crime arrest rates",
    "United States, 1973"
  ) +
  expand_limits(x = c(-.4)) +
  guides(color = FALSE)
