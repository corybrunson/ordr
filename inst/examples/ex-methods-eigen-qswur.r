# subset QS data to rank variables
qs_ranks <- subset(
  qswur_usa,
  complete.cases(qswur_usa),
  select = 8:13
)
head(qs_ranks)

# eigendecomposition of Kendall correlation matrix
qs_ranks %>%
  cor(method = "kendall") %>%
  eigen() %>%
  print() -> qs_eigen

# recover eigenvectors
get_rows(qs_eigen)
identical(get_cols(qs_eigen), get_rows(qs_eigen))

# wrap as a 'tbl_ord'
as_tbl_ord(qs_eigen)

# same eigendecomposition, preserving row names and adding column names
qs_ranks %>%
  cor(method = "kendall") %>%
  eigen_ord() %>%
  print() -> qs_eigen

# wrap as a 'tbl_ord' and augment with dimension names
augment_ord(as_tbl_ord(qs_eigen))

# decomposition returns pure eigenvectors
get_conference(qs_eigen)
