# https://stats.stackexchange.com/a/133806
# https://stats.stackexchange.com/a/288646

data(wine, package = "ggbiplot")

wine %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  cbind_rows(class = wine.class) %>%
  mutate_cols(variable = dplyr::row_number()) ->
  wine_pca

wine %>%
  factanal(factors = 2L, scores = "regression", rotation = "none") %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  cbind_rows(class = wine.class) %>%
  mutate_cols(variable = dplyr::row_number()) ->
  wine_fa

wine_pca %>%
  confer_inertia("rows") %>%
  ggbiplot(sec.axes = "cols", scale.factor = 6) +
  geom_rows_point(aes(color = class)) +
  geom_unit_circle() +
  geom_cols_vector(arrow = NULL) +
  geom_cols_text(aes(label = variable))

wine_fa %>%
  confer_inertia("rows") %>%
  ggbiplot(sec.axes = "cols", scale.factor = 6) +
  geom_rows_point(aes(color = class)) +
  geom_unit_circle() +
  geom_cols_vector(arrow = NULL) +
  geom_cols_text(aes(label = variable))
