devtools::load_all()

data(country_differences, country_attributes)
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

# MDS vs PCA

m1 <- cmdscale(x1, k = 3)
m2 <- prcomp(x1)
# doesn't produce a rotation matrix
X1 <- m1$points
X2 <- m2$x
d <- min(ncol(X1), ncol(X2))
D1 <- diag(1 / apply(X1[, 1:d, drop = FALSE], 2, sd))
D2 <- diag(1 / apply(X2[, 1:d, drop = FALSE], 2, sd))
S <- solve(t(X1[, 1:d, drop = FALSE]) %*% X2[, 1:d, drop = FALSE])
R <- solve(D2) %*% S %*% solve(D1)



# PCA vs PCA

base_pca <- prcomp(t(x1))
pca <- prcomp(t(-x2))
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

x <- make_bibble(
  u = pca$x,
  v = pca$rotation,
  coordinates = tibble(.name = colnames(pca$x))
)
y <- make_bibble(
  u = base_pca$x,
  v = base_pca$rotation,
  coordinates = tibble(.name = colnames(base_pca$x))
)
