# eigendecompose covariance matrix of ability tests
gi_eigen <- eigen(ability.cov$cov)

# recover eigenvectors
get_rows(gi_eigen)
identical(get_cols(gi_eigen), get_rows(gi_eigen))

# wrap as a 'tbl_ord'
as_tbl_ord(gi_eigen)

# same eigendecomposition, preserving names
gi_eigen <- eigen_ord(ability.cov$cov)

# wrap as a 'tbl_ord' and augment with dimension names
augment_ord(as_tbl_ord(gi_eigen))

# decomposition returns pure eigenvectors
get_conference(gi_eigen)
