d <- mtcars[seq(6L), ]
names(d) <- paste0("..coord", seq(ncol(d)))
m <- cmdscale_ord(dist(d), k = 3L)
m <- as_tbl_ord(m)

test_that("all available 'StatSpantree' engines work", {
  n_mst <- (nrow(m) - 1L) * 2L
  {
    skip_if_not_installed("mlpack")
    p <- ggbiplot(m) + stat_spantree(ord_aes(m), engine = "mlpack")
    expect_equal(nrow(layer_data(p)), n_mst)
  }
  {
    skip_if_not_installed("vegan")
    p <- ggbiplot(m) + stat_spantree(ord_aes(m), engine = "vegan")
    expect_equal(nrow(layer_data(p)), n_mst)
  }
  {
    skip_if_not_installed("ade4")
    p <- ggbiplot(m) + stat_spantree(ord_aes(m), engine = "ade4")
    expect_equal(nrow(layer_data(p)), n_mst)
  }
})
