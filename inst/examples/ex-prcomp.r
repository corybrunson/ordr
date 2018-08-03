# principal components analysis ('prcomp') examples

devtools::load_all()

# calculate several 'prcomp's
x <- USPersonalExpenditure
p <- prcomp(x, center = FALSE, scale = FALSE)
p2 <- prcomp(x, center = TRUE, scale = FALSE)
p3 <- prcomp(x, center = TRUE, scale = TRUE)

# access the 'U' and 'V' matrices
recover_u.prcomp(p)
recover_v.prcomp(p)
recover_u.prcomp(p2)
recover_v.prcomp(p2)
recover_u.prcomp(p3)
recover_v.prcomp(p3)

# reconstruct approximations of the original data
reconstruct.prcomp(p)
reconstruct.prcomp(p2)
reconstruct.prcomp(p3)

# check that the distances between the original and recovered values are small
hist(as.matrix(x) - reconstruct.prcomp(p))
hist(as.matrix(x) - reconstruct.prcomp(p2))
hist(as.matrix(x) - reconstruct.prcomp(p3))

# wrap 'p', 'p2', and 'p3' as 'tbl_ord' objects
b <- as_tbl_ord.prcomp(p)
b2 <- as_tbl_ord.prcomp(p2)
b3 <- as_tbl_ord.prcomp(p3)

# access the names of the artificial coordinates
recover_coord.prcomp(b)
recover_coord.prcomp(b2)
recover_coord.prcomp(b3)

# augment methods
augment_u.prcomp(b)
augment_v.prcomp(b)
augment_coord.prcomp(b)
augment_u.prcomp(b2)
augment_v.prcomp(b2)
augment_coord.prcomp(b2)
augment_u.prcomp(b3)
augment_v.prcomp(b3)
augment_coord.prcomp(b3)

# data frames
fortify(b)
fortify(b2)
fortify(b3)

# biplot of scores and loadings
gg <- ggbiplot(b) +
  geom_u_point() +
  geom_v_vector() +
  theme(aspect.ratio = 1)

# view biplot
gg

# add radiating text for loadings
gg + geom_v_text_radiate(aes(label = .name))
