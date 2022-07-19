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
# recover canonical weights
get_rows(savings_cancor, elements = "active")
get_cols(savings_cancor, elements = "active")
head(get_rows(savings_cancor, elements = "supplementary"))
head(get_cols(savings_cancor, elements = "supplementary"))
# augment canonical weights with row names and centers
(savings_cancor <- augment_ord(savings_cancor))
# column-standard biplot of structure correlations
savings_cancor %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name), sec.axes = "rows", scale.factor = 1/2,
           elements = "active") +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_point(color = "forestgreen") +
  geom_rows_text_repel(color = "forestgreen") +
  expand_limits(x = c(-.05, .05))
