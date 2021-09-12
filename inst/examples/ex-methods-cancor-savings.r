# data frame of life-cycle savings across countries
class(LifeCycleSavings)
head(LifeCycleSavings)
# canonical correlation analysis of age distributions and financial factors
savings_cancor <- candisc::cancor(
  LifeCycleSavings[, c("pop15", "pop75")],
  LifeCycleSavings[, c("sr", "dpi", "ddpi")]
)
# wrap as a 'tbl_ord' object
(savings_cancor <- as_tbl_ord(savings_cancor))
# summarize ordination
glance(savings_cancor)
# recover canonical weights
get_rows(savings_cancor, .supplement = FALSE)
get_cols(savings_cancor, .supplement = FALSE)
head(get_rows(savings_cancor, .supplement = TRUE))
head(get_cols(savings_cancor, .supplement = TRUE))
# augment canonical weights with row names and centers
(savings_cancor <- augment_ord(savings_cancor))
# summarize canonical correlations
tidy(savings_cancor)
# scree plot of canonical correlations
tidy(savings_cancor) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  geom_col() +
  labs(x = "", y = "Inertia (squared canonical correlations)")
# fortification binds tibbles of canonical weights
fortify(savings_cancor)
# row-standard biplot of structure correlations
savings_cancor %>%
  confer_inertia("cols") %>%
  ggbiplot(aes(label = .name, color = .matrix),
           sec.axes = "cols", scale.factor = 10, .supplement = FALSE) +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_rows_vector() +
  geom_rows_text_radiate() +
  geom_cols_point() +
  geom_cols_text_repel() +
  scale_color_brewer(type = "qual") +
  expand_limits(x = c(-.1, .1))
# column-standard biplot of structure correlations
savings_cancor %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name, color = .matrix),
           sec.axes = "rows", scale.factor = 1/2, .supplement = FALSE) +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_point() +
  geom_rows_text_repel() +
  scale_color_brewer(type = "qual") +
  expand_limits(x = c(-.05, .05))
# symmetric biplot of structure correlations
savings_cancor %>%
  confer_inertia("symmetric") %>%
  ggbiplot(aes(label = .name, color = .matrix), .supplement = FALSE) +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_vector() +
  geom_rows_text_radiate() +
  scale_color_brewer(type = "qual") +
  expand_limits(x = c(-1, 1), y = c(-.75, .75))
