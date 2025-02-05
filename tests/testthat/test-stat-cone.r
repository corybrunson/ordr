d <- mtcars[, c("wt", "mpg")]
d$name <- rownames(mtcars)
d0 <- rbind(d, 0)
# actual cone (excluding origin)
cone_corners <- d0[setdiff(chull(d0[, c("wt", "mpg")]), nrow(d0)), "name", ]
# plot template
p <- ggplot(d, aes(wt, mpg, label = name))

test_that("`stat_cone()` faithfully computes the cone", {
  l <- layer_data(p + stat_cone(geom = "text"))
  expect_setequal(l$label, cone_corners)
})

test_that("`stat_cone(origin = TRUE)` includes exactly one origin point", {
  l <- layer_data(p + stat_cone(origin = TRUE, geom = "text"))
  expect_setequal(l$label, c(cone_corners, NA))
  expect_equal(length(cone_corners), nrow(l) - 1L)
  expect_setequal(l[is.na(l$label), c("x", "y")], 0)
})
