cities_ord <- as_tbl_ord(cmdscale_ord(UScitiesD, k = 2L))

# FIXME: Un-comment tests once EVD class methods are revised.

test_that("`ord_quality()` correctly subsets by rank", {
  cities_quality <- ord_quality(cities_ord)
  cities_quality_1 <- ord_quality(cities_ord, rank = 1)
  expect_equal(cities_quality[1], cities_quality_1)
})

test_that("`ord_adequacy()` correctly subsets by rank", {
  cities_adeq <- ord_adequacy(cities_ord, .matrix = "rows")
  cities_adeq_1 <- ord_adequacy(cities_ord, .matrix = "rows", rank = 1)
  expect_equal(cities_adeq[,  1L, drop = FALSE], cities_adeq_1)
})

# test_that("`ord_adequacy()` accommodates missing items", {
#   cities_v_adeq <- ord_adequacy(cities_ord, .matrix = "cols")
#   expect_equal(nrow(cities_v_adeq), 0L)
# })

test_that("`ord_predictivity()` correctly subsets by rank", {
  cities_predictiv <- ord_predictivity(cities_ord, .matrix = "rows")
  cities_predictiv_1 <- ord_predictivity(cities_ord, .matrix = "rows", rank = 1)
  expect_equal(cities_predictiv[,  1L, drop = FALSE], cities_predictiv_1)
})

# test_that("`ord_predictivity()` accommodates missing items", {
#   cities_v_predictiv <- ord_predictivity(cities_ord, .matrix = "cols")
#   expect_equal(nrow(cities_v_predictiv), 0L)
# })
