# note: behavior depends on installed packages with class-specific methods
# class 'prcomp'
iris[, -5] %>%
  prcomp(scale = TRUE) %>%
  as_tbl_ord() %>%
  print() -> iris_pca
plot(iris_pca)
screeplot(iris_pca)
biplot(iris_pca)
# class 'ca'
ca::smoke %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> smoke_ca
plot(smoke_ca)
screeplot(smoke_ca)
biplot(smoke_ca)
# class 'lra'
USArrests %>%
  subset(select = -UrbanPop) %>%
  lra() %>%
  as_tbl_ord() %>%
  print() -> arrests_lra
plot(arrests_lra)
screeplot(arrests_lra)
biplot(arrests_lra)
