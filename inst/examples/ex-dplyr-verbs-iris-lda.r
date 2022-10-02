# illustrative ordination: LDA of iris data
(iris_lda <- ordinate(iris, cols = 1:4, lda_ord, grouping = iris$Species))

# extract a coordinate or annotation
head(pull_rows(iris_lda, Species))
pull_cols(iris_lda, LD2)

# rename an annotation
rename_cols(iris_lda, species = name)

# select annotations
select_rows(iris_lda, species = name, .element)

# create, modify, and delete annotations
mutate_cols(iris_lda, vec.length = sqrt(LD1^2 + LD2^2))
transmute_cols(iris_lda, vec.length = sqrt(LD1^2 + LD2^2))

# bind data frames of annotations
iris_medians <-
  stats::aggregate(iris[, 1:4], median, by = iris[, 5, drop = FALSE])
iris_lda %>%
  # retain '.element' in order to match by `elements`
  select_rows(.element) %>%
  cbind_rows(iris_medians, elements = "active")
