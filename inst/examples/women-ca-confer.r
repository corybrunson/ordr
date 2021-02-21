# Confer inertia between rows and columns of CA on Spanish ISSP sample
data(issp_women)
issp_women %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> issp_ca
# Reproduce Exhibit 9.2 in Greenacre (2010)...
issp_ca %>%
  confer_inertia(c(1, 1)) %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_rows_point(color = "darkgreen") +
  geom_rows_text_repel(color = "darkgreen") +
  geom_cols_point(color = "brown", shape = 17) +
  geom_cols_text_repel(color = "brown")
# Reproduce Exhibit 9.3 in Greenacre (2010)...
issp_ca %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_rows_point(color = "darkgreen") +
  geom_cols_point(color = "brown", shape = 17) +
  geom_cols_text_repel(color = "brown")
# Reproduce Exhibits 9.4 and 9.6 in Greenacre (2010)...
