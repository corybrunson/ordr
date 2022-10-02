# data frame of Anderson iris species measurements
class(iris)
head(iris)

# default (unstandardized discriminant) coefficients
lda_ord(iris[, 1:4], iris[, 5]) %>%
  as_tbl_ord() %>%
  print() -> iris_lda

# recover centroid coordinates and measurement discriminant coefficients
get_rows(iris_lda, elements = "active")
head(get_rows(iris_lda, elements = "score"))
get_cols(iris_lda)

# augment ordination with centroid and measurement names
augment_ord(iris_lda)
