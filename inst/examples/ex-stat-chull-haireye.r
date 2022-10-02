# correspondence analysis of combined female and male hair and eye color data
HairEyeColor %>%
  rowSums(dims = 2L) %>%
  MASS::corresp(nf = 2L) %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> hec_ca
# inertia across artificial coordinates (all singular values < 1)
get_inertia(hec_ca)

# in row-principal biplot, row coordinates are weighted averages of columns
hec_ca %>%
  confer_inertia("rows") %>%
  ggbiplot(aes(color = .matrix, fill = .matrix, shape = .matrix)) +
  theme_bw() +
  stat_cols_chull(alpha = .1) +
  geom_cols_point() +
  geom_rows_point() +
  ggtitle("Row-principal CA of hair & eye color")
# in column-principal biplot, column coordinates are weighted averages of rows
hec_ca %>%
  confer_inertia("cols") %>%
  ggbiplot(aes(color = .matrix, fill = .matrix, shape = .matrix)) +
  theme_bw() +
  stat_rows_chull(alpha = .1) +
  geom_rows_point() +
  geom_cols_point() +
  ggtitle("Column-principal CA of hair & eye color")
