# illustrative ordination: FA of Swiss social data
swiss_fa <- factanal(swiss, factors = 3L, scores = "regression")
print(swiss_fa)

# add the 'tbl_ord' wrapper
swiss_fa_ord <- as_tbl_ord(swiss_fa)
# inspect wrapped model
is_tbl_ord(swiss_fa_ord)
print(swiss_fa_ord)
valid_tbl_ord(swiss_fa_ord)
# unwrap the model
un_tbl_ord(swiss_fa_ord)

# create a 'tbl_ord' directly from row and column factors
# (missing inertia & other attributes)
swiss_fa_ord2 <- make_tbl_ord(rows = swiss_fa$scores, cols = swiss_fa$loadings)
# inspect wrapped factors
is_tbl_ord(swiss_fa_ord2)
print(swiss_fa_ord2)
valid_tbl_ord(swiss_fa_ord2)
# unwrap factors
un_tbl_ord(swiss_fa_ord2)
