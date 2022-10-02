(pca <- ordinate(iris, cols = 1:4, prcomp))
ggbiplot(pca) + geom_rows_point() + geom_cols_vector()

# manually negate second coordinate
(pca_neg <- negate_ord(pca, 2))
ggbiplot(pca_neg) + geom_rows_point() + geom_cols_vector()

# NB: 'prcomp' method takes precedence; negations are part of the wrapper
biplot(pca)
biplot(pca_neg)

# negate to the first orthant
(pca_orth <- negate_to_first_orthant(pca, "v"))
get_negation(pca_orth)
