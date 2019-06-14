# singular value decomposition examples

# calculate a SVD
x <- USArrests
s <- svd_ord(x)

# wrap `s` as a 'tbl_ord' object
b <- as_tbl_ord(s)

# get the raw left and right matrices (scores and loadings)
u <- recover_u(b)
print(u)
v <- recover_v(b)
print(v)
print(u %*% t(v))

# get (or introduce) the names of the artifical coordinates (maybe 'SV1', etc.)
recover_coord(b)

# get the inertia (the variances in the directions of the singular vectors)
recover_inertia(b)

# combine the scores, loadings, and coordinates with any info of the same length
# should include '.names'
augment_u(b)
augment_v(b)
augmentation_coord(b)

# reconstruct (approximately) the original data
reconstruct(b)
hist(as.matrix(x) - reconstruct(b))

# data frame of combined scores and loadings
fortify(b)

# biplot of scores and loadings
gg <- ggbiplot(b) +
  geom_u_point() +
  geom_v_vector()

# view biplot
gg

# add radiating text for loadings
gg + geom_v_text_radiate(aes(label = .name))
