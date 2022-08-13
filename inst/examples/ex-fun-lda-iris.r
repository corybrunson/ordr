# Anderson iris species data centroid
iris_centroid <- t(apply(iris[, 1:4], 2, mean))
# unstandardized discriminant coefficients: the discriminant axes are linear
# combinations of the centered variables
iris_lda <- lda_ord(iris[, 1:4], iris[, 5], axes.scale = "unstandardized")
# linear combinations of centered variables
print(sweep(iris_lda$means, 2, iris_centroid, "-") %*% get_cols(iris_lda))
# discriminant centroids
print(get_rows(iris_lda, elements = "active"))

# unstandardized coefficient LDA biplot
iris_lda %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  mutate_rows(
    species = grouping,
    discriminant = ifelse(.element == "active", "centroid", "case")
  ) %>%
  ggbiplot() +
  theme_bw() +
  geom_rows_point(aes(
    color = grouping,
    size = discriminant, alpha = discriminant
  )) +
  geom_cols_vector(color = "#888888") +
  geom_cols_text_radiate(aes(label = name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Unstandardized coefficient biplot of iris LDA") +
  expand_limits(y = c(-3, 5))

# standardized discriminant coefficients: permit comparisons across the
# variables
iris_lda <- lda_ord(iris[, 1:4], iris[, 5], axes.scale = "standardized")
# standardized variable contributions to discriminant axes
iris_lda %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  fortify(.matrix = "cols") %>%
  dplyr::mutate(variable = name) %>%
  tidyr::gather(discriminant, coefficient, LD1, LD2) %>%
  ggplot(aes(x = discriminant, y = coefficient, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(y = "Standardized coefficient", x = "Linear discriminant") +
  theme_bw() +
  coord_flip()
# standardized coefficient LDA biplot
iris_lda %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  mutate_rows(
    species = grouping,
    discriminant = ifelse(.element == "active", "centroid", "case")
  ) %>%
  ggbiplot() +
  theme_bw() +
  geom_rows_point(aes(
    color = grouping,
    size = discriminant, alpha = discriminant
  )) +
  geom_cols_vector(color = "#888888") +
  geom_cols_text_radiate(aes(label = name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Standardized coefficient biplot of iris LDA") +
  expand_limits(y = c(-2, 3))

# variable contributions (de-sphered discriminant coefficients): recover the
# inner product relationship with the centered class centroids
iris_lda <- lda_ord(iris[, 1:4], iris[, 5], axes.scale = "contribution")
# symmetric square root of within-class covariance
C_W_eig <- eigen(cov(iris[, 1:4] - iris_lda$means[iris[, 5], ]))
C_W_sqrtinv <-
  C_W_eig$vectors %*% diag(1/sqrt(C_W_eig$values)) %*% t(C_W_eig$vectors)
# product of matrix factors (scores and loadings)
print(get_rows(iris_lda, elements = "active") %*% t(get_cols(iris_lda)))
# "asymmetric" square roots of Mahalanobis distances between variables
print(sweep(iris_lda$means, 2, iris_centroid, "-") %*% C_W_sqrtinv)
# contribution LDA biplot
iris_lda %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  mutate_rows(
    species = grouping,
    discriminant = ifelse(.element == "active", "centroid", "case")
  ) %>%
  ggbiplot() +
  theme_bw() +
  geom_rows_point(aes(
    color = grouping,
    size = discriminant, alpha = discriminant
  )) +
  geom_cols_vector(color = "#888888") +
  geom_cols_text_radiate(aes(label = name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Contribution biplot of iris LDA") +
  expand_limits(y = c(-2, 3.5))
