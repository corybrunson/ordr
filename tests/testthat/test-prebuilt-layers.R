test_that("prebuilt layer shadowing blocks namespace fallback", {
  prebuilt <- grep(
    "^(stat|geom)_(rows|cols)_",
    getNamespaceExports("ordr"),
    value = TRUE
  )
  skip_if(length(prebuilt) == 0L, "No prebuilt layer exports to shadow.")

  env <- new.env(parent = rlang::caller_env())
  shadowed <- shadow_prebuilt_layers(env)
  expect_setequal(shadowed, prebuilt)
  expect_error(env[[prebuilt[[1L]]]](), "must not be used")
})

test_that("prebuilt layer shadowing preserves local constructors", {
  prebuilt <- grep(
    "^(stat|geom)_(rows|cols)_",
    getNamespaceExports("ordr"),
    value = TRUE
  )
  skip_if(length(prebuilt) < 2L, "Fewer than two prebuilt layer exports.")

  target <- prebuilt[[1L]]
  other <- prebuilt[[2L]]
  env <- new.env(parent = rlang::caller_env())
  env[[target]] <- function(...) "generated"
  shadowed <- shadow_prebuilt_layers(env)

  expect_false(target %in% shadowed)
  expect_identical(env[[target]](), "generated")
  expect_error(env[[other]](), "must not be used")
})
