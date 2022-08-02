fit_lm0 <- lm(data = mtcars, formula = mpg ~ wt + hp + as.factor(cyl) + 0)
fit_lm1 <- lm(data = mtcars, formula = mpg ~ wt + hp + as.factor(cyl))
fit_lm2 <- lm(as.matrix(mtcars[, "mpg"]) ~
                as.matrix(mtcars[, c("wt", "hp")]))
fit_lm3 <- lm(as.matrix(mtcars[, "mpg"]) ~
                as.matrix(mtcars[, "wt"]) +
                as.matrix(mtcars[, "hp"]))

test_that("'lm' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_lm0)), ncol(get_cols(fit_lm0)))
  expect_equal(ncol(get_rows(fit_lm1)), ncol(get_cols(fit_lm1)))
  expect_equal(ncol(get_rows(fit_lm2)), ncol(get_cols(fit_lm2)))
  expect_equal(ncol(get_rows(fit_lm3)), ncol(get_cols(fit_lm3)))
})

test_that("'lm' inertia cannot be accessed", {
  expect_equal(recover_inertia(fit_lm0), NA_real_)
})

test_that("'lm' does not confer inertia", {
  expect_null(recover_conference(fit_lm0))
})

test_that("'lm' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_lm0)),
               ".element" %in% names(recover_aug_cols(fit_lm0)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lm1)),
               ".element" %in% names(recover_aug_cols(fit_lm1)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lm2)),
               ".element" %in% names(recover_aug_cols(fit_lm2)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lm3)),
               ".element" %in% names(recover_aug_cols(fit_lm3)))
})

test_that("`as_tbl_ord()` coerces 'lm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm3)))
})

fit_mlm0 <- lm(as.matrix(mtcars[, c("mpg", "qsec")]) ~
                 as.matrix(mtcars[, c("wt", "hp")]) + 0)
fit_mlm1 <- lm(as.matrix(mtcars[, c("mpg", "qsec")]) ~
                 as.matrix(mtcars[, c("wt", "hp")]))
fit_mlm2 <- lm(as.matrix(mtcars[, c("mpg", "qsec")]) ~
                 wt + hp, data = mtcars)
fit_mlm3 <- lm(as.matrix(mtcars[, c("mpg", "qsec")]) ~
                 as.matrix(mtcars[, "wt"]) +
                 as.matrix(mtcars[, "hp"]))

test_that("'mlm' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_mlm0)), ncol(get_cols(fit_mlm0)))
  expect_equal(ncol(get_rows(fit_mlm1)), ncol(get_cols(fit_mlm1)))
  expect_equal(ncol(get_rows(fit_mlm2)), ncol(get_cols(fit_mlm2)))
  expect_equal(ncol(get_rows(fit_mlm3)), ncol(get_cols(fit_mlm3)))
})

test_that("'mlm' inertia cannot be accessed", {
  expect_equal(recover_inertia(fit_mlm0), NA_real_)
})

test_that("'mlm' does not confer inertia", {
  expect_null(recover_conference(fit_mlm0))
})

test_that("'mlm' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_mlm0)),
               ".element" %in% names(recover_aug_cols(fit_mlm0)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_mlm1)),
               ".element" %in% names(recover_aug_cols(fit_mlm1)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_mlm2)),
               ".element" %in% names(recover_aug_cols(fit_mlm2)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_mlm3)),
               ".element" %in% names(recover_aug_cols(fit_mlm3)))
})

test_that("`as_tbl_ord()` coerces 'mlm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm3)))
})

fit_glm0 <- glm(data = mtcars, formula = gear ~ wt + hp + 0, family = poisson)
fit_glm1 <- glm(data = mtcars, formula = gear ~ wt + hp, family = poisson)
fit_glm2 <- glm(as.matrix(mtcars[, "gear"]) ~
                  as.matrix(mtcars[, c("wt", "hp")]),
                family = poisson)
fit_glm3 <- glm(as.matrix(mtcars[, "gear"]) ~
                  as.matrix(mtcars[, "wt"]) +
                  as.matrix(mtcars[, "hp"]),
                family = poisson)

test_that("'glm' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_glm0)), ncol(get_cols(fit_glm0)))
  expect_equal(ncol(get_rows(fit_glm1)), ncol(get_cols(fit_glm1)))
  expect_equal(ncol(get_rows(fit_glm2)), ncol(get_cols(fit_glm2)))
  expect_equal(ncol(get_rows(fit_glm3)), ncol(get_cols(fit_glm3)))
})

test_that("'glm' inertia cannot be accessed", {
  expect_equal(recover_inertia(fit_glm0), NA_real_)
})

test_that("'glm' does not confer inertia", {
  expect_null(recover_conference(fit_glm0))
})

test_that("'glm' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_glm0)),
               ".element" %in% names(recover_aug_cols(fit_glm0)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_glm1)),
               ".element" %in% names(recover_aug_cols(fit_glm1)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_glm2)),
               ".element" %in% names(recover_aug_cols(fit_glm2)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_glm3)),
               ".element" %in% names(recover_aug_cols(fit_glm3)))
})

test_that("`as_tbl_ord()` coerces 'glm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm3)))
})
