cities_ord <- as_tbl_ord(cmdscale_ord(UScitiesD, k = 2L))

# FIXME: Un-comment tests once EVD class methods are revised.

test_that("`ord_quality()` correctly subsets by rank", {
  cities_quality <- ord_quality(cities_ord)
  cities_quality_1 <- ord_quality(cities_ord, rank = 1)
  expect_equal(cities_quality[1], cities_quality_1)
})

test_that("`ord_adequacy()` agrees with matrix calculation", {
  cities_adeq <- ord_adequacy(cities_ord, .matrix = "rows")
  p <- recover_conference(cities_ord)[1L]
  l <- recover_inertia(cities_ord)
  u <- recover_rows(cities_ord)
  u <- u %*% diag(1 / sqrt(l) ^ p)
  a1 <- diag( u[, 1, drop = FALSE] %*% t(u[, 1, drop = FALSE]) )
  a2 <- diag( u %*% t(u) )
  expect_equivalent(cities_adeq, cbind(a1, a2))
})

test_that("`ord_adequacy()` correctly subsets by rank", {
  cities_adeq <- ord_adequacy(cities_ord, .matrix = "rows")
  cities_adeq_1 <- ord_adequacy(cities_ord, .matrix = "rows", rank = 1)
  expect_equal(cities_adeq[,  1L, drop = FALSE], cities_adeq_1)
})

test_that("`ord_adequacy()` accommodates item absence", {
  cities_v_adeq <- ord_adequacy(cities_ord, .matrix = "cols")
  expect_equal(nrow(cities_v_adeq), 0L)
})

test_that("`ord_predictivity()` agrees with matrix calculation", {
  cities_predictiv <- ord_predictivity(cities_ord, .matrix = "rows")
  p <- recover_conference(cities_ord)[1L]
  l <- recover_inertia(cities_ord)
  u <- recover_rows(cities_ord)
  u <- u %*% diag(1 / sqrt(l) ^ p)
  p1 <- 
    diag( u[, 1, drop = FALSE] %*% matrix(l[1]) %*% t(u[, 1, drop = FALSE]) ) /
    diag( u %*% diag(l) %*% t(u) )
  p2 <- 
    diag( u %*% diag(l) %*% t(u) ) /
    diag( u %*% diag(l) %*% t(u) )
  expect_equivalent(cities_predictiv, cbind(p1, p2))
})

test_that("`ord_predictivity()` correctly subsets by rank", {
  cities_predictiv <- ord_predictivity(cities_ord, .matrix = "rows")
  cities_predictiv_1 <- ord_predictivity(cities_ord, .matrix = "rows", rank = 1)
  expect_equal(cities_predictiv[,  1L, drop = FALSE], cities_predictiv_1)
})

test_that("`ord_predictivity()` accommodates item absence", {
  cities_v_predictiv <- ord_predictivity(cities_ord, .matrix = "cols")
  expect_equal(nrow(cities_v_predictiv), 0L)
})
