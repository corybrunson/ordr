# matrix of U.S. personal expenditure data
class(USPersonalExpenditure)
print(USPersonalExpenditure)
# singular value decomposition into row and column coordinates
USPersonalExpenditure %>%
  svd_ord() %>%
  as_tbl_ord() %>%
  print() -> spend_svd

# recover matrices of row and column coordinates
get_rows(spend_svd)
get_cols(spend_svd)

# augment with row and column names
augment_ord(spend_svd)
# initial matrix decomposition confers no inertia to coordinates
get_conference(spend_svd)
