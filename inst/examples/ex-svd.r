# singular value decomposition examples

devtools::load_all()

# calculate a SVD
x <- USArrests
s <- svd(x)

# initial implementation

# access the singular values
get_singular_values(s)
get_diagonal_matrix(s)

# error checks
get_singular_values(x)

# recover the original data
head(recompose_data_matrix(s))
head(x)
# check that the differences between the original and recovered values are small
hist(as.matrix(x) - recompose_data_matrix(s))

# methods implementation

# wrap `s` as a 'tbl_ord' object
b <- as_tbl_ord(s)
# inspecting the object directly should produce an error because
# the `print` method depends on the `recover_*` and `augment_*` methods below
b

# get the raw left and right matrices (scores and loadings)
u <- recover_u(b)
print(u)
v <- recover_v(b)
print(v)
print(u %*% t(v))

# get (or introduce) the names of the artifical coordinates (maybe 'SV1', etc.)
recover_coord(b)

# combine the scores, loadings, and coordinates with any info of the same length
# should include '.names'
augment_u(b)
augment_v(b)
augment_coord(b)

# the print method should now work
b

# reconstruct (approximately) the original data
reconstruct(b)
hist(as.matrix(x) - reconstruct(b))

# data frame of combined scores and loadings
fortify(b)

# biplot of scores and loadings
gg <- ggbiplot(b) +
  geom_u_point() +
  geom_v_vector()
# add radiating text for loadings
gg + geom_v_text_radiate()
