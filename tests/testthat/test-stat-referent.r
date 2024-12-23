# disjoint data sets with common columns
df1 <- mtcars[seq(6), c("hp", "wt")]
df2 <- mtcars[seq(7, 10), c("hp", "wt")]

test_that("only inherited data influences plotting window", {
  p1 <- ggplot(df1, aes(x = hp/100, y = wt)) +
    coord_equal() +
    geom_point()
  p1r2 <- ggplot(df1, aes(x = hp/100, y = wt)) +
    coord_equal() +
    stat_referent(referent = df2)
  p12 <- ggplot(rbind(df1, df2), aes(x = hp/100, y = wt)) +
    coord_equal() +
    geom_point()
  
  # different inherited data results in different ranges
  expect_false(isTRUE(all.equal(
    layer_scales(p1)$x$range$range,
    layer_scales(p12)$x$range$range
  )))
  expect_false(isTRUE(all.equal(
    layer_scales(p1)$y$range$range,
    layer_scales(p12)$y$range$range
  )))
  # new referent data results in same ranges
  expect_equal(layer_scales(p1)$x$range$range, layer_scales(p1r2)$x$range$range)
  expect_equal(layer_scales(p1)$y$range$range, layer_scales(p1r2)$y$range$range)
})

test_that("reference data does not affect computation in base layer", {
  expect_equal(
    StatReferent$compute_group(df1) |> head(n = 2),
    StatReferent$compute_group(df1, referent = df2) |> head(n = 2)
  )
})

test_that("mapping and referent parameters together yield new plotting data", {
  df2_setup <- StatReferent$setup_params(
    df1,
    list(mapping = aes(x = hp/100, y = wt), referent = df2)
  )$referent
  expect_equal(names(df2_setup), c("x", "y", "angle", "radius"))
  expect_equal(nrow(df2_setup), 4L)
})
