# data frame of Anderson iris species measurements
class(iris)
head(iris)

# compute scaled row-principal components of scaled measurements
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca

# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
get_cols(iris_pca)

# augment measurements with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
