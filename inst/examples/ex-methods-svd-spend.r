# matrix of U.S. personal expenditure data
class(USPersonalExpenditure)
print(USPersonalExpenditure)
# singular value decomposition into row and column coordinates
USPersonalExpenditure %>%
  svd_ord() %>%
  as_tbl_ord() %>%
  print() -> spend_svd
# summarize ordination
glance(spend_svd)
# recover matrices of row and column coordinates
get_rows(spend_svd)
get_cols(spend_svd)
# augment with row and column names
augment_ord(spend_svd)
# summarize artifical coordinates
tidy(spend_svd)
# scree plot of inertia
tidy(spend_svd) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# fortification automatically augments coordinates
fortify(spend_svd)
# initial matrix decomposition confers no inertia to coordinates
get_conference(spend_svd)
# row-principal biplot, allowing radiating text to exceed plotting window
spend_svd %>%
  confer_inertia(1) %>%
  ggbiplot(aes(label = .name), clip = "off",
           sec.axes = "cols", scale.factor = 50) +
  geom_rows_label(size = 3) +
  geom_cols_vector() +
  # omit labels in the hull with the origin
  geom_cols_text_radiate(stat = "cone") +
  ggtitle(
    "U.S. Personal Expenditure data, 1940-1960",
    "Row-principal biplot of SVD"
  )
