# data frame of life-cycle savings across countries
class(LifeCycleSavings)
head(LifeCycleSavings)
# canonical correlation analysis of age distributions and financial factors
savings_cca <- PMA::CCA(
  LifeCycleSavings[, c(2L, 3L)],
  LifeCycleSavings[, c(1L, 4L, 5L)],
  K = 2L, penaltyx = .7, penaltyz = .7,
  xnames = names(LifeCycleSavings)[c(2L, 3L)],
  znames = names(LifeCycleSavings)[c(1L, 4L, 5L)]
)
# wrap as a 'tbl_ord' object
(savings_cca <- as_tbl_ord(savings_cca))
# summarize ordination
glance(savings_cca)
# recover canonical variates
get_rows(savings_cca)
get_cols(savings_cca)
# augment canonical variates with variable names
(savings_cca <- augment_ord(savings_cca))
# summarize canonical correlations
tidy(savings_cca)
# scree plot of canonical correlations
tidy(savings_cca) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  geom_col() +
  labs(x = "", y = "Inertia (squared canonical correlations)")
# fortification binds tibbles of canonical variates
fortify(savings_cca)
# column-standard biplot of non-zero canonical variates
nz_rows <- which(apply(recover_rows(savings_cca) != 0, 1L, any))
nz_cols <- which(apply(recover_cols(savings_cca) != 0, 1L, any))
savings_cca %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name, color = .matrix)) +
  theme_bw() +
  geom_origin() +
  geom_unit_circle(linetype = "dotted") +
  geom_cols_vector(subset = nz_cols) +
  geom_cols_text_radiate(subset = nz_cols) +
  geom_rows_point(subset = nz_rows) +
  geom_rows_text_repel(subset = nz_rows) +
  expand_limits(x = c(-1, 1), y = c(-1, 1))
