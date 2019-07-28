# a 'tbl_ord' object
pca <- as_tbl_ord(prcomp(USArrests))
pca <- confer_inertia(pca, "symmetric")
# fortify/tidy a 'tbl_ord' object
fortify(pca)
tidy(pca)
# scores or loadings plots using `ggplot()`
ggplot(pca, aes(x = PC1, y = PC2), .matrix = "u") + geom_point()
ggplot(pca, aes(x = PC1, y = PC2), .matrix = "v") + geom_point()
# biplot using `ggplot()`
ggplot(pca, aes(x = PC1, y = PC2)) + geom_point(aes(color = .matrix))
# fortify to coordinates & scree plot using `ggplot()`
fortify(pca, .matrix = "coord")
ggplot(pca, .matrix = "coord", aes(x = .name, y = .sdev^2)) +
  geom_bar(stat = "identity")
ggplot(pca, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  geom_bar(stat = "identity")
# `fortify()` being called internally by `ggbiplot()`
ggbiplot(pca) + geom_v_vector() + geom_u_point()
