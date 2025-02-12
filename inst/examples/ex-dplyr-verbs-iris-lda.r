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
# TODO: Requirement of `.elements` for matching is fragile.
iris_lda %>%
  # retain '.element' in order to match by `elements`
  select_rows(.element) %>%
  cbind_rows(iris_medians, elements = "active")
iris_lda %>%
  select_rows(name, Species) %>%
  left_join_rows(iris_medians, by = c("name" = "Species"))
