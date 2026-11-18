library(testthat)
library(vdiffr)

iris_ord <- iris[, 1:4] |>
  prcomp(scale = TRUE) |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord() |>
  ordr::mutate_rows(species = iris$Species)

hair_eye_ord <- HairEyeColor |>
  rowSums(dims = 2L) |>
  MASS::corresp(nf = 2L) |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord() |>
  ordr::confer_inertia("rows")

judge_ord <- USJudgeRatings[, -c(1, 12)] |>
  princomp() |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord()

spend_ord <- USPersonalExpenditure |>
  prcomp() |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord()

glass_ord <- MASS::lda(Site ~ SiO2 + Al2O3 + FeO + MgO + CaO, glass) |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord() |>
  ordr::confer_inertia(1)

euro_ord <- eurodist |>
  ordr::cmdscale_ord(k = 11) |>
  ordr::as_tbl_ord() |>
  ordr::negate_ord("PCo2") |>
  ordr::augment_ord()

savings_ord <- ordr::cancor_ord(
  LifeCycleSavings[, c("pop15", "pop75")],
  LifeCycleSavings[, c("sr", "dpi", "ddpi")],
  scores = TRUE
) |>
  ordr::as_tbl_ord() |>
  ordr::augment_ord() |>
  ordr::confer_inertia("cols")

interpolation_data <- iris[c(42, 61, 110), seq(5, 1), drop = FALSE]
interpolation_data[3L, "Sepal.Width"] <- NA
interpolation_data[1L, "Petal.Length"] <- NA

test_that("stat_density_2d synth", {
  adapt_layer_to_biplot(
    ggplot2::stat_density_2d, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_density_2d_synth_rows",
      ggbiplot(savings_ord) +
        stat_rows_density_2d(contour = TRUE, alpha = .5)
    )
  )
})

test_that("stat_density_2d parse", {
  adapt_layer_to_biplot(
    ggplot2::stat_density_2d, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_density_2d_parse_rows",
      ggbiplot(savings_ord) +
        stat_rows_density_2d(contour = TRUE, alpha = .5)
    )
  )
})

test_that("stat_density_2d_filled synth", {
  adapt_layer_to_biplot(
    ggplot2::stat_density_2d_filled, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_density_2d_filled_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_density_2d_filled(alpha = .75)
    )
  )
})

test_that("stat_density_2d_filled parse", {
  adapt_layer_to_biplot(
    ggplot2::stat_density_2d_filled, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_density_2d_filled_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_density_2d_filled(alpha = .75)
    )
  )
})

test_that("stat_ellipse synth", {
  adapt_layer_to_biplot(
    ggplot2::stat_ellipse, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_ellipse_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_ellipse(alpha = .25)
    )
  )
})

test_that("stat_ellipse parse", {
  adapt_layer_to_biplot(
    ggplot2::stat_ellipse, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_ellipse_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_ellipse(alpha = .25)
    )
  )
})

test_that("stat_center synth", {
  adapt_layer_to_biplot(
    gggda::stat_center, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_center_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_center(size = 3, alpha = .5)
    )
  )
})

test_that("stat_center parse", {
  adapt_layer_to_biplot(
    gggda::stat_center, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_center_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_center(size = 3, alpha = .5)
    )
  )
})

test_that("stat_star synth", {
  adapt_layer_to_biplot(
    gggda::stat_star, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_star_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_star(alpha = .25)
    )
  )
})

test_that("stat_star parse", {
  adapt_layer_to_biplot(
    gggda::stat_star, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_star_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_star(alpha = .25)
    )
  )
})

test_that("stat_chull synth", {
  adapt_layer_to_biplot(gggda::stat_chull, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_chull_synth_cols",
      ggbiplot(hair_eye_ord) +
        stat_cols_chull(alpha = .1)
    )
  )
})

test_that("stat_chull parse", {
  adapt_layer_to_biplot(gggda::stat_chull, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_chull_parse_cols",
      ggbiplot(hair_eye_ord) +
        stat_cols_chull(alpha = .1)
    )
  )
})

# FIXME: Render plot elements.

test_that("stat_peel synth", {
  adapt_layer_to_biplot(gggda::stat_peel, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_peel_synth_rows",
      ggbiplot(judge_ord) +
        stat_rows_peel(
          elements = "score",
          aes(alpha = after_stat(hull)), num = 3L
        )
    )
  )
})

test_that("stat_peel parse", {
  adapt_layer_to_biplot(gggda::stat_peel, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_peel_parse_rows",
      ggbiplot(judge_ord) +
        stat_rows_peel(
          elements = "score",
          aes(alpha = after_stat(hull)), num = 3L
        )
    )
  )
})

test_that("stat_cone synth", {
  adapt_layer_to_biplot(gggda::stat_cone, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_cone_synth_cols",
      ggbiplot(spend_ord) +
        stat_cols_cone(linetype = "dotted")
    )
  )
})

test_that("stat_cone parse", {
  adapt_layer_to_biplot(gggda::stat_cone, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_cone_parse_cols",
      ggbiplot(spend_ord) +
        stat_cols_cone(linetype = "dotted")
    )
  )
})

test_that("stat_depth synth", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_depth, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_depth_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_depth(n = 20L)
    )
  )
})

test_that("stat_depth parse", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_depth, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_depth_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_depth(n = 20L)
    )
  )
})

test_that("stat_depth_filled synth", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_depth_filled, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_depth_filled_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_depth_filled(alpha = .75)
    )
  )
})

test_that("stat_depth_filled parse", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_depth_filled, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_depth_filled_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_depth_filled(alpha = .75)
    )
  )
})

test_that("stat_scale synth", {
  adapt_layer_to_biplot(
    gggda::stat_scale, method = "synth", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_scale_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_scale(mult = 2)
    )
  )
})

test_that("stat_scale parse", {
  adapt_layer_to_biplot(
    gggda::stat_scale, method = "parse", planar = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_scale_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_scale(mult = 2)
    )
  )
})

test_that("stat_spantree synth", {
  skip_if_not_installed("mlpack")

  adapt_layer_to_biplot(gggda::stat_spantree, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_spantree_synth_rows",
      ggbiplot(euro_ord) +
        stat_rows_spantree(
          engine = "mlpack", alpha = .5, linetype = "dotted"
        )
    )
  )
})

test_that("stat_spantree parse", {
  skip_if_not_installed("mlpack")

  adapt_layer_to_biplot(gggda::stat_spantree, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_spantree_parse_rows",
      ggbiplot(euro_ord) +
        stat_rows_spantree(
          engine = "mlpack", alpha = .5, linetype = "dotted"
        )
    )
  )
})

test_that("stat_bagplot synth", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_bagplot, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_bagplot_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_bagplot()
    )
  )
})

test_that("stat_bagplot parse", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::stat_bagplot, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_bagplot_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_bagplot()
    )
  )
})

test_that("stat_rule synth", {
  adapt_layer_to_biplot(
    gggda::stat_rule,
    method = "synth", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_rule_synth_cols",
      ggbiplot(glass_ord) +
        stat_cols_rule(aes(label = name), num = 8L)
    )
  )
})

test_that("stat_rule parse", {
  adapt_layer_to_biplot(
    gggda::stat_rule,
    method = "parse", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_rule_parse_cols",
      ggbiplot(glass_ord) +
        stat_cols_rule(aes(label = name), num = 8L)
    )
  )
})

test_that("stat_projection synth", {
  adapt_layer_to_biplot(
    ordr::stat_projection,
    method = "synth", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_projection_synth_rows",
      ggbiplot(iris_ord) +
        stat_rows_projection(subset = c(1L, 51L, 101L))
    )
  )
})

test_that("stat_projection parse", {
  adapt_layer_to_biplot(
    ordr::stat_projection,
    method = "parse", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "stat_projection_parse_rows",
      ggbiplot(iris_ord) +
        stat_rows_projection(subset = c(1L, 51L, 101L))
    )
  )
})

test_that("geom_point synth", {
  adapt_layer_to_biplot(ggplot2::geom_point, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_point_synth_rows",
      ggbiplot(iris_ord) +
        geom_rows_point(alpha = .5)
    )
  )
})

test_that("geom_point parse", {
  adapt_layer_to_biplot(ggplot2::geom_point, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_point_parse_rows",
      ggbiplot(iris_ord) +
        geom_rows_point(alpha = .5)
    )
  )
})

test_that("geom_path synth", {
  adapt_layer_to_biplot(ggplot2::geom_path, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_path_synth_rows",
      ggbiplot(hair_eye_ord) +
        geom_rows_path() +
        geom_cols_path(linetype = "dotted")
    )
  )
})

test_that("geom_path parse", {
  adapt_layer_to_biplot(ggplot2::geom_path, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_path_parse_rows",
      ggbiplot(hair_eye_ord) +
        geom_rows_path() +
        geom_cols_path(linetype = "dotted")
    )
  )
})

test_that("geom_polygon synth", {
  adapt_layer_to_biplot(ggplot2::geom_polygon, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_polygon_synth_cols",
      ggbiplot(hair_eye_ord) +
        geom_cols_polygon(aes(group = 1), alpha = .25)
    )
  )
})

test_that("geom_polygon parse", {
  adapt_layer_to_biplot(ggplot2::geom_polygon, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_polygon_parse_cols",
      ggbiplot(hair_eye_ord) +
        geom_cols_polygon(aes(group = 1), alpha = .25)
    )
  )
})

test_that("geom_contour synth", {
  adapt_layer_to_biplot(ggplot2::geom_contour, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_contour_synth_rows",
      ggbiplot(iris_ord) +
        geom_rows_contour(stat = "depth", contour = TRUE)
    )
  )
})

test_that("geom_contour parse", {
  adapt_layer_to_biplot(ggplot2::geom_contour, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_contour_parse_rows",
      ggbiplot(iris_ord) +
        geom_rows_contour(stat = "depth", contour = TRUE)
    )
  )
})

test_that("geom_density_2d synth", {
  adapt_layer_to_biplot(ggplot2::geom_density_2d, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_density_2d_synth_rows",
      ggbiplot(savings_ord) +
        geom_rows_density_2d(contour = TRUE, alpha = .5)
    )
  )
})

test_that("geom_density_2d parse", {
  adapt_layer_to_biplot(ggplot2::geom_density_2d, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_density_2d_parse_rows",
      ggbiplot(savings_ord) +
        geom_rows_density_2d(contour = TRUE, alpha = .5)
    )
  )
})

test_that("geom_density_2d_filled synth", {
  adapt_layer_to_biplot(
    ggplot2::geom_density_2d_filled, method = "synth"
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_density_2d_filled_synth_rows",
      ggbiplot(iris_ord) +
        geom_rows_density_2d_filled(contour = TRUE, alpha = .75)
    )
  )
})

test_that("geom_density_2d_filled parse", {
  adapt_layer_to_biplot(
    ggplot2::geom_density_2d_filled, method = "parse"
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_density_2d_filled_parse_rows",
      ggbiplot(iris_ord) +
        geom_rows_density_2d_filled(contour = TRUE, alpha = .75)
    )
  )
})

test_that("geom_text synth", {
  adapt_layer_to_biplot(ggplot2::geom_text, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_text_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_text(aes(label = name), size = 3)
    )
  )
})

test_that("geom_text parse", {
  adapt_layer_to_biplot(ggplot2::geom_text, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_text_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_text(aes(label = name), size = 3)
    )
  )
})

test_that("geom_label synth", {
  adapt_layer_to_biplot(ggplot2::geom_label, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_label_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_label(aes(label = name), size = 3)
    )
  )
})

test_that("geom_label parse", {
  adapt_layer_to_biplot(ggplot2::geom_label, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_label_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_label(aes(label = name), size = 3)
    )
  )
})

test_that("geom_text_repel synth", {
  adapt_layer_to_biplot(ggrepel::geom_text_repel, method = "synth")
  shadow_prebuilt_layers()
  set.seed(1)
  suppressWarnings(
    expect_doppelganger(
      "geom_text_repel_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_text_repel(
          aes(label = name), seed = 1, max.time = Inf
        )
    )
  )
})

test_that("geom_text_repel parse", {
  adapt_layer_to_biplot(ggrepel::geom_text_repel, method = "parse")
  shadow_prebuilt_layers()
  set.seed(1)
  suppressWarnings(
    expect_doppelganger(
      "geom_text_repel_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_text_repel(
          aes(label = name), seed = 1, max.time = Inf
        )
    )
  )
})

test_that("geom_label_repel synth", {
  adapt_layer_to_biplot(ggrepel::geom_label_repel, method = "synth")
  shadow_prebuilt_layers()
  set.seed(1)
  suppressWarnings(
    expect_doppelganger(
      "geom_label_repel_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_label_repel(
          aes(label = name), seed = 1, max.time = Inf
        )
    )
  )
})

test_that("geom_label_repel parse", {
  adapt_layer_to_biplot(ggrepel::geom_label_repel, method = "parse")
  shadow_prebuilt_layers()
  set.seed(1)
  suppressWarnings(
    expect_doppelganger(
      "geom_label_repel_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_label_repel(
          aes(label = name), seed = 1, max.time = Inf
        )
    )
  )
})

test_that("geom_axis synth", {
  adapt_layer_to_biplot(gggda::geom_axis, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_axis_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_axis(
          aes(label = name, center = 0, scale = 1),
          axis.type = "predictive"
        )
    )
  )
})

test_that("geom_axis parse", {
  adapt_layer_to_biplot(gggda::geom_axis, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_axis_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_axis(
          aes(label = name, center = 0, scale = 1),
          axis.type = "predictive"
        )
    )
  )
})

test_that("geom_pointranges synth", {
  adapt_layer_to_biplot(gggda::geom_pointranges, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_pointranges_synth_rows",
      ggbiplot(glass_ord) +
        geom_rows_pointranges(
          aes(center = 0, scale = 1), alpha = .5
        )
    )
  )
})

test_that("geom_pointranges parse", {
  adapt_layer_to_biplot(gggda::geom_pointranges, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_pointranges_parse_rows",
      ggbiplot(glass_ord) +
        geom_rows_pointranges(
          aes(center = 0, scale = 1), alpha = .5
        )
    )
  )
})

test_that("geom_lineranges synth", {
  adapt_layer_to_biplot(gggda::geom_lineranges, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_lineranges_synth_rows",
      ggbiplot(glass_ord) +
        geom_rows_lineranges(
          aes(center = 0, scale = 1), alpha = .5
        )
    )
  )
})

test_that("geom_lineranges parse", {
  adapt_layer_to_biplot(gggda::geom_lineranges, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_lineranges_parse_rows",
      ggbiplot(glass_ord) +
        geom_rows_lineranges(
          aes(center = 0, scale = 1), alpha = .5
        )
    )
  )
})

test_that("geom_isoline synth", {
  adapt_layer_to_biplot(gggda::geom_isoline, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_isoline_synth_cols",
      ggbiplot(glass_ord) +
        geom_cols_isoline(
          aes(center = 0, scale = 1), alpha = .25, num = 4L
        )
    )
  )
})

test_that("geom_isoline parse", {
  adapt_layer_to_biplot(gggda::geom_isoline, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_isoline_parse_cols",
      ggbiplot(glass_ord) +
        geom_cols_isoline(
          aes(center = 0, scale = 1), alpha = .25, num = 4L
        )
    )
  )
})

test_that("geom_text_radiate synth", {
  adapt_layer_to_biplot(gggda::geom_text_radiate, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_text_radiate_synth_cols",
      ggbiplot(spend_ord) +
        geom_cols_text_radiate(aes(label = name), size = 3)
    )
  )
})

test_that("geom_text_radiate parse", {
  adapt_layer_to_biplot(gggda::geom_text_radiate, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_text_radiate_parse_cols",
      ggbiplot(spend_ord) +
        geom_cols_text_radiate(aes(label = name), size = 3)
    )
  )
})

test_that("geom_vector synth", {
  adapt_layer_to_biplot(gggda::geom_vector, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_vector_synth_cols",
      ggbiplot(iris_ord) +
        geom_cols_vector(aes(label = name), color = "#444444")
    )
  )
})

test_that("geom_vector parse", {
  adapt_layer_to_biplot(gggda::geom_vector, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_vector_parse_cols",
      ggbiplot(iris_ord) +
        geom_cols_vector(aes(label = name), color = "#444444")
    )
  )
})

test_that("geom_bagplot synth", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::geom_bagplot, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_bagplot_synth_rows",
      ggbiplot(iris_ord) +
        geom_rows_bagplot()
    )
  )
})

test_that("geom_bagplot parse", {
  skip_if_not_installed("ddalpha")

  adapt_layer_to_biplot(gggda::geom_bagplot, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_bagplot_parse_rows",
      ggbiplot(iris_ord) +
        geom_rows_bagplot()
    )
  )
})

test_that("geom_rule synth", {
  adapt_layer_to_biplot(
    gggda::geom_rule,
    method = "synth", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_rule_synth_cols",
      ggbiplot(glass_ord) +
        geom_cols_rule(
          aes(
            center = 0, scale = 1, lower = 0, upper = 1, label = name
          ),
          axis_labels = FALSE, axis_ticks = FALSE, axis_text = FALSE
        )
    )
  )
})

test_that("geom_rule parse", {
  adapt_layer_to_biplot(
    gggda::geom_rule,
    method = "parse", planar = TRUE, referential = TRUE
  )
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_rule_parse_cols",
      ggbiplot(glass_ord) +
        geom_cols_rule(
          aes(
            center = 0, scale = 1, lower = 0, upper = 1, label = name
          ),
          axis_labels = FALSE, axis_ticks = FALSE, axis_text = FALSE
        )
    )
  )
})

test_that("geom_interpolation synth", {
  adapt_layer_to_biplot(ordr::geom_interpolation, method = "synth")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_interpolation_synth_cols",
      ggbiplot(iris_ord) +
        geom_point(stat = rows_stat("identity"), alpha = .25) +
        geom_cols_interpolation(
          aes(center = center, scale = scale, interpolate = name),
          new_data = interpolation_data, type = "centroid", alpha = .5
        )
    )
  )
})

test_that("geom_interpolation parse", {
  adapt_layer_to_biplot(ordr::geom_interpolation, method = "parse")
  shadow_prebuilt_layers()
  suppressWarnings(
    expect_doppelganger(
      "geom_interpolation_parse_cols",
      ggbiplot(iris_ord) +
        geom_point(stat = rows_stat("identity"), alpha = .25) +
        geom_cols_interpolation(
          aes(center = center, scale = scale, interpolate = name),
          new_data = interpolation_data, type = "centroid", alpha = .5
        )
    )
  )
})
