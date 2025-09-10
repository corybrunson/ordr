iris_pca <- ordinate(iris[1:4], prcomp)

# single value applies to both factors
print(iris_pca, n = 10)

# double values apply to factors in order
print(iris_pca, n = c(6, 2))

# use `list()` to pass `NULL` (for default) to only one factor
print(iris_pca, n = list(2, NULL))
print(iris_pca, n = list(NULL, 2))
