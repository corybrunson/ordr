# data frame of life-cycle savings across countries
class(LifeCycleSavings)
head(LifeCycleSavings)
# canonical correlation analysis of age distributions and financial factors
savings_cancor <- cancor_ord(
  LifeCycleSavings[, c("pop15", "pop75")],
  LifeCycleSavings[, c("sr", "dpi", "ddpi")]
)
# wrap as a 'tbl_ord' object
(savings_cancor <- as_tbl_ord(savings_cancor))
# summarize ordination
glance(savings_cancor)
# recover canonical weights
get_rows(savings_cancor)
get_cols(savings_cancor)
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
# row-standard biplot of canonical weights
savings_cancor %>%
  confer_inertia("cols") %>%
  ggbiplot(aes(label = .name), sec.axes = "cols", scale.factor = 2) +
  geom_unit_circle() +
  geom_rows_vector() +
  geom_rows_text_radiate() +
  geom_cols_point(color = "forestgreen") +
  geom_cols_text_repel(color = "forestgreen") +
  expand_limits(x = c(-.1, .1))
# column-standard biplot of canonical weights
savings_cancor %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name), sec.axes = "rows", scale.factor = .5) +
  geom_unit_circle() +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_point(color = "forestgreen") +
  geom_rows_text_repel(color = "forestgreen") +
  expand_limits(x = c(-.1, .1))
