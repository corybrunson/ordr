# principal components analysis ('princomp') examples

devtools::load_all()

# calculate a 'princomp'
x <- USArrests
p <- princomp(USArrests)

# access the 'U' and 'V' matrices
recover_u.princomp(p)
recover_v.princomp(p)

# reconstruct an approximation of the original data
reconstruct.princomp(p)

# check that the distances between the original and recovered values are small
hist(as.matrix(x) - reconstruct.princomp(p))

# access the names of the artificial coordinates
recover_coord.princomp(p)

# augment methods
augment_u.princomp(p)
augment_v.princomp(p)
augment_coord.princomp(p)

# wrap 'p' as a 'tbl_ord' object
# this cannot be done correctly with the 'as_tbl_ord_default' method
b <- as_tbl_ord.princomp(p)

# print 'b'
# this will throw an error
b

# pass b to 'fortify'
# this will throw an error
fortify(b)
