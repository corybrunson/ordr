
data(country_differences, country_attributes)
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

base_pca <- prcomp(t(x1))
pca <- prcomp(t(x2))
# number of shared dimensions
d <- min(ncol(pca$rotation), ncol(base_pca$rotation))
# identify common variables (coordinates)
coords <- intersect(rownames(pca$rotation), rownames(base_pca$rotation))
# signs of dot products of loadings from both PCAs projected onto these
signs <- (pca$rotation[coords, 1:d] * base_pca$rotation[coords, 1:d]) %>%
  apply(2, sum) %>% sign()
# reverse directions of misaligned PCs
pca$rotation[, 1:d] <- sweep(pca$rotation[, 1:d], 2, signs, "/")
if ("x" %in% names(pca)) pca$x[, 1:d] <- sweep(pca$x[, 1:d], 2, signs, "/")
# return updated PCA
pca
