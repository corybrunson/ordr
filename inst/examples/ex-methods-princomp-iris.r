# data frame of Anderson iris species measurements
class(iris)
head(iris)

# compute unscaled row-principal components of scaled measurements
iris[, -5] %>%
  princomp() %>%
  as_tbl_ord() %>%
  print() -> iris_pca

# recover observation principal coordinates and measurement standard coordinates
head(get_rows(iris_pca))
get_cols(iris_pca)

# augment measurement coordinates with names and scaling parameters
(iris_pca <- augment_ord(iris_pca))
