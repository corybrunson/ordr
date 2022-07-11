# data frame of Swiss fertility and socioeconomic indicators
class(swiss)
head(swiss)
# perform factor analysis
swiss_fa <- factanal(~ ., factors = 2L, data = swiss, scores = "regression")
# wrap as a 'tbl_ord' object
(swiss_fa <- as_tbl_ord(swiss_fa))
# summarize ordination
glance(swiss_fa)
# recover loadings
get_rows(swiss_fa, .supplement = FALSE)
get_cols(swiss_fa)
head(get_rows(swiss_fa, .supplement = TRUE), n = 12L)
# augment column loadings with uniquenesses
(swiss_fa <- augment_ord(swiss_fa))
# summarize factors
tidy(swiss_fa)
# scree plot of factor variances
tidy(swiss_fa) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  geom_col() +
  labs(x = "", y = "Variance")
# symmetric biplot 
swiss_fa %>%
  ggbiplot() +
  theme_bw() +
  geom_cols_vector(aes(color = .uniqueness)) +
  geom_cols_text_radiate(aes(label = .name)) +
  expand_limits(x = c(-2, 2.5), y = c(-1.5, 2))
# row-standard biplot with loadings in standard coordinates and regression
# scores overlaid
swiss_fa %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(label = .name), .supplement = TRUE) +
  theme_bw() +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_unit_circle() +
  geom_rows_point(aes(alpha = .supplement)) +
  scale_alpha_manual(values = c(0, 1), guide = "none")
