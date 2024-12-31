# data frame of life-cycle savings across countries
class(LifeCycleSavings)
head(LifeCycleSavings)
savings_pop <- LifeCycleSavings[, c("pop15", "pop75")]
savings_oec <- LifeCycleSavings[, c("sr", "dpi", "ddpi")]

# canonical correlation analysis with scores and correlations included
savings_cca <- cancor_ord(savings_pop, savings_oec, scores = TRUE)
savings_cca <- augment_ord(as_tbl_ord(savings_cca))
head(get_cols(savings_cca))
head(get_cols(savings_cca, elements = "score"))
get_rows(savings_cca, elements = "structure")
get_cols(savings_cca, elements = "structure")

# biplot of interset and intraset correlations with the population data
# NB: `contour = TRUE` is not automatically set as in `geom_density_2d()`
savings_cca %>%
  confer_inertia("cols") %>%
  ggbiplot(aes(label = name, color = .matrix)) +
  theme_bw() + theme_scaffold() +
  geom_unit_circle() +
  geom_rows_density_2d(elements = "score", color = "grey", contour = TRUE) +
  geom_rows_vector(arrow = NULL, elements = "structure") +
  geom_cols_vector(arrow = NULL, elements = "structure", linetype = "dashed") +
  geom_rows_text(elements = "structure", hjust = "outward") +
  geom_cols_text(elements = "structure", hjust = "outward") +
  scale_color_brewer(limits = c("rows", "cols"), type = "qual") +
  expand_limits(x = c(-1, 1), y = c(-1, 1))

# situate country scores along financial variables
savings_cca %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = name)) +
  theme_scaffold() +
  geom_cols_axis(elements = "active") +
  geom_rows_text(elements = "score")
