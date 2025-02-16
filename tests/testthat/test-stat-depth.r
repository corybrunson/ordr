skip_if_not_installed("ddalpha")

# adapted from `test-stat-density2d.R`
test_that("`stat_depth()` can produce contour and raster data", {
  p <- ggplot(faithful, aes(eruptions, waiting))
  
  p_raster <- p + stat_depth(contour = FALSE)
  p_lines <- p + stat_depth()
  p_bands <- p + stat_depth_filled()
  
  # expected computed variables and types
  
  d_raster <- layer_data(p_raster)
  expect_true("depth" %in% names(d_raster))
  expect_true("ndepth" %in% names(d_raster))
  expect_true("n" %in% names(d_raster))
  expect_true(unique(d_raster$level) == 1)
  expect_true(unique(d_raster$piece) == 1)
  
  d_lines <- layer_data(p_lines)
  expect_true("level" %in% names(d_lines))
  expect_true("nlevel" %in% names(d_lines))
  expect_true(is.numeric(d_lines$level))
  
  d_bands <- layer_data(p_bands)
  expect_true("level" %in% names(d_bands))
  expect_true("nlevel" %in% names(d_bands))
  expect_true(is.ordered(d_bands$level))
  
  # agreement between lines and bands
  
  p_raster2 <- p + stat_depth_filled(contour = FALSE)
  d_raster2 <- layer_data(p_raster2)
  expect_identical(d_raster$x, d_raster2$x)
  expect_identical(d_raster$y, d_raster2$y)
  expect_equal(d_raster$depth, d_raster2$depth)
  expect_equal(d_raster$ndepth, d_raster2$ndepth)
  expect_identical(d_raster$n, d_raster2$n)
  
  # agreement between (theoretically) equivalent compositions
  
  p_lines2 <- ggplot(d_raster, aes(x, y, z = depth)) + stat_contour()
  d_lines2 <- layer_data(p_lines2)
  expect_identical(d_lines$x, d_lines2$x)
  expect_identical(d_lines$y, d_lines2$y)
  expect_identical(d_lines$piece, d_lines2$piece)
  expect_identical(d_lines$group, d_lines2$group)
  expect_identical(d_lines$level, d_lines2$level)
  
  p_bands2 <- ggplot(d_raster, aes(x, y, z = depth)) + stat_contour_filled()
  d_bands2 <- layer_data(p_bands2)
  expect_identical(d_bands$x, d_bands2$x)
  expect_identical(d_bands$y, d_bands2$y)
  expect_identical(d_bands$piece, d_bands2$piece)
  expect_identical(d_bands$group, d_bands2$group)
  expect_identical(d_bands$level, d_bands2$level)
  expect_identical(d_bands$level_mid, d_bands2$level_mid)
  
  p_lines3 <- p + stat_depth(contour_var = "ndepth")
  d_lines3 <- layer_data(p_lines3)
  p_lines4 <- ggplot(d_raster, aes(x, y, z = ndepth)) + stat_contour()
  d_lines4 <- layer_data(p_lines4)
  expect_identical(d_lines3$x, d_lines4$x)
  expect_identical(d_lines3$y, d_lines4$y)
  expect_identical(d_lines3$piece, d_lines4$piece)
  expect_identical(d_lines3$group, d_lines4$group)
  expect_identical(d_lines3$level, d_lines4$level)
  
})
