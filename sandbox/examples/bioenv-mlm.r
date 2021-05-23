# Generalized multiple linear regression on marine ecosystem data
# Reproduce Exhibit 2.5 in Greenacre (2010)
data(bioenv)
bioenv_std <- dplyr::mutate_if(bioenv, is.numeric, ~ as.vector(scale(.)))
resp_std <- as.matrix(dplyr::select(bioenv_std, a:e))
pred_std <- as.matrix(dplyr::select(bioenv_std, x = Depth, y = Pollution))
lm(resp_std ~ pred_std + 0) %>%
  as_tbl_ord() %>%
  print() -> bioenv_std_mlm
ggbiplot(bioenv_std_mlm, aes(label = .name)) +
  theme_bw() +
  geom_rows_text(color = "darkgreen") +
  geom_cols_vector(color = "brown4") +
  geom_cols_text_radiate(color = "brown4")
# Reproduce Exhibit 3.2 in Greenacre (2010)
bioenv %>%
  dplyr::mutate_at(dplyr::vars(a:e), ~ . ^ (1/4)) %>%
  dplyr::mutate_at(dplyr::vars(Pollution:Depth), ~ as.vector(scale(.))) %>%
  print() -> bioenv_4rt
resp_4rt <- as.matrix(dplyr::select(bioenv_4rt, a:e))
pred_4rt <- as.matrix(dplyr::select(bioenv_4rt, x = Depth, y = Pollution))
lm(resp_4rt ~ pred_4rt + 0) %>%
  as_tbl_ord() %>%
  print() -> bioenv_4rt_mlm
ggbiplot(bioenv_4rt_mlm, aes(x = x, y = y, label = .name)) +
  theme_bw() +
  geom_rows_text(color = "darkgreen") +
  geom_cols_vector(color = "brown4") +
  geom_cols_text_radiate(color = "brown4")
