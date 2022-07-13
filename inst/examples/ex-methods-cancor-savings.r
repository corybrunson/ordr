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
head(get_rows(savings_cancor))
head(get_cols(savings_cancor))
# augment canonical weights with row names and centers
(savings_cancor <- augment_ord(savings_cancor))
# row-standard biplot of structure correlations
savings_cancor %>%
  confer_inertia("cols") %>%
  ggbiplot(aes(label = .name, color = .matrix), elements = "active") +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_rows_vector() +
  geom_rows_text_radiate() +
  geom_cols_point() +
  geom_cols_text_repel() +
  scale_color_brewer(limits = c("rows", "cols"), type = "qual") +
  expand_limits(x = c(-1, 1), y = c(-1, 1))
# column-standard biplot of structure correlations
savings_cancor %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name, color = .matrix), elements = "active") +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_point() +
  geom_rows_text_repel() +
  scale_color_brewer(limits = c("rows", "cols"), type = "qual") +
  expand_limits(x = c(-1, 1), y = c(-1, 1))
# symmetric biplot of structure correlations
savings_cancor %>%
  confer_inertia("symmetric") %>%
  ggbiplot(aes(label = .name, color = .matrix), elements = "active") +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_rows_vector() +
  geom_rows_text_radiate() +
  scale_color_brewer(limits = c("rows", "cols"), type = "qual") +
  expand_limits(x = c(-1, 1), y = c(-1, 1))
