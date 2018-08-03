# principal components analysis ('prcomp') examples

devtools::load_all()

# calculate several 'prcomp's
x <- USPersonalExpenditure
p <- prcomp(x, center = FALSE, scale = FALSE)
p2 <- prcomp(x, center = TRUE, scale = FALSE)
p3 <- prcomp(x, center = TRUE, scale = TRUE)

# access the 'U' and 'V' matrices
recover_u(p)
recover_v(p)
recover_u(p2)
recover_v(p2)
recover_u(p3)
recover_v(p3)

# reconstruct approximations of the original data
reconstruct(p)
reconstruct(p2)
reconstruct(p3)

# check that the distances between the original and recovered values are small
hist(as.matrix(x) - reconstruct(p))
hist(as.matrix(x) - reconstruct(p2))
hist(as.matrix(x) - reconstruct(p3))

# wrap 'p', 'p2', and 'p3' as 'tbl_ord' objects
b <- as_tbl_ord(p)
b2 <- as_tbl_ord(p2)
b3 <- as_tbl_ord(p3)

# access the names of the artificial coordinates
recover_coord(b)
recover_coord(b2)
recover_coord(b3)

# augment methods
augment_u(b)
augment_v(b)
augment_coord(b)
augment_u(b2)
augment_v(b2)
augment_coord(b2)
augment_u(b3)
augment_v(b3)
augment_coord(b3)

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
