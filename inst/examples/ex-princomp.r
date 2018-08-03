# principal components analysis ('princomp') examples

devtools::load_all()

# calculate a 'princomp'
x <- USArrests
p <- princomp(USArrests)

# access the 'U' and 'V' matrices
recover_u(p)
recover_v(p)

# reconstruct an approximation of the original data
reconstruct(p)

# check that the distances between the original and recovered values are small
hist(as.matrix(x) - reconstruct(p))

# access the names of the artificial coordinates
recover_coord(p)

# augment methods
augment_u(p)
augment_v(p)
augment_coord(p)

# wrap 'p' as a 'tbl_ord' object
b <- as_tbl_ord(p)

# print 'b'
b

# pass b to 'fortify'
fortify(b)

# biplot of scores and loadings
gg <- ggbiplot(b) +
  geom_u_point() +
  geom_v_vector() +
  theme(aspect.ratio = 1)

# view biplot
gg
