test_that("geom constructors are created and work", {
  # clear environment
  suppressWarnings(
    rm(list = c("geom_rows_point", "geom_cols_point"), envir = .GlobalEnv)
  )
  
  adapt_layer_to_biplot(geom_point, env = .GlobalEnv)
  
  # check constructor creation
  expect_true(exists("geom_rows_point", envir = .GlobalEnv))
  expect_true(is.function(geom_rows_point))
  expect_true(exists("geom_cols_point", envir = .GlobalEnv))
  expect_true(is.function(geom_cols_point))

  # check that calls returns layers with appropriate stats
  l_rows <- get("geom_rows_point", envir = .GlobalEnv)()
  expect_true(inherits(l_rows, "LayerInstance"))
  expect_true("stat" %in% names(l_rows))
  expect_true(inherits(l_rows$stat, "StatRows"))
  l_cols <- get("geom_cols_point", envir = .GlobalEnv)()
  expect_true(inherits(l_cols, "LayerInstance"))
  expect_true("stat" %in% names(l_cols))
  expect_true(inherits(l_cols$stat, "StatCols"))
})

test_that("stat ggproto and constructors are created", {
  # clear environment
  suppressWarnings(rm(
    list = c(
      "StatRowsEllipse",
      "StatColsEllipse",
      "stat_rows_ellipse",
      "stat_cols_ellipse"
    ),
    envir = .GlobalEnv
  ))
  
  adapt_layer_to_biplot(stat_ellipse, env = .GlobalEnv)

  # check ggproto creation
  expect_true(exists("StatRowsEllipse", envir = .GlobalEnv))
  expect_true(inherits(get("StatRowsEllipse"), "ggproto"))
  expect_true(exists("StatColsEllipse", envir = .GlobalEnv))
  expect_true(inherits(get("StatColsEllipse"), "ggproto"))

  # check constructor creation
  expect_true(exists("stat_rows_ellipse", envir = .GlobalEnv))
  expect_true(is.function(stat_rows_ellipse))
  expect_true(exists("stat_cols_ellipse", envir = .GlobalEnv))
  expect_true(is.function(stat_cols_ellipse))

  # check that calls returns layers with ggprotos as stats
  l_rows <- get("stat_rows_ellipse", envir = .GlobalEnv)()
  expect_true(inherits(l_rows, "LayerInstance"))
  expect_true("stat" %in% names(l_rows))
  expect_true(inherits(l_rows$stat, "StatRowsEllipse"))
  l_cols <- get("stat_cols_ellipse", envir = .GlobalEnv)()
  expect_true(inherits(l_cols, "LayerInstance"))
  expect_true("stat" %in% names(l_cols))
  expect_true(inherits(l_cols$stat, "StatColsEllipse"))
})

test_that("stat constructors print symbolic ggproto names", {
  suppressWarnings(rm(
    list = c(
      "StatRowsProjection",
      "StatColsProjection",
      "stat_rows_projection",
      "stat_cols_projection"
    ),
    envir = .GlobalEnv
  ))

  adapt_layer_to_biplot(stat_projection, env = .GlobalEnv)

  body_text <- paste(
    deparse(body(get("stat_rows_projection", envir = .GlobalEnv))),
    collapse = "\n"
  )
  expect_match(body_text, "stat = StatRowsProjection", fixed = TRUE)
  expect_false(grepl("<environment>", body_text, fixed = TRUE))
})

test_that("ggproto object inputs are used directly", {
  suppressWarnings(rm(
    list = c(
      "StatRowsEllipse",
      "StatColsEllipse",
      "stat_rows_ellipse",
      "stat_cols_ellipse"
    ),
    envir = .GlobalEnv
  ))

  adapt_layer_to_biplot(ggplot2::StatEllipse, env = .GlobalEnv)

  rows_ggproto <- get("StatRowsEllipse", envir = .GlobalEnv)
  expect_true(inherits(rows_ggproto, "StatRowsEllipse"))
  expect_identical(
    rows_ggproto$required_aes,
    ggplot2::StatEllipse$required_aes
  )
})

test_that("ggproto class name strings are resolved", {
  namespaced_env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot("ggplot2::StatEllipse", env = namespaced_env)
  expect_true(exists("stat_rows_ellipse", envir = namespaced_env))
  expect_true(inherits(
    get("StatRowsEllipse", envir = namespaced_env),
    "StatRowsEllipse"
  ))

  bare_env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot("StatEllipse", env = bare_env)
  expect_true(exists("stat_cols_ellipse", envir = bare_env))
})

test_that("ggproto and constructor names must match", {
  env <- rlang::new_environment(parent = globalenv())
  assign(
    "StatMismatch",
    ggplot2::ggproto("StatOther", ggplot2::Stat, required_aes = character()),
    envir = env
  )
  expect_error(
    ordr:::parse_layer_spec("StatMismatch", quote(StatMismatch), env),
    "does not match"
  )
})

test_that("density layer naming preserves constructor spelling", {
  define_underscore <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::stat_density_2d, env = define_underscore)
  adapt_layer_to_biplot(ggplot2::geom_density_2d, env = define_underscore)
  expect_true(exists(
    "stat_rows_density_2d", envir = define_underscore, inherits = FALSE
  ))
  expect_true(exists(
    "stat_cols_density_2d", envir = define_underscore, inherits = FALSE
  ))
  expect_true(exists(
    "geom_rows_density_2d", envir = define_underscore, inherits = FALSE
  ))
  expect_false(exists(
    "stat_rows_density2d", envir = define_underscore, inherits = FALSE
  ))
  expect_true("StatRowsDensity2d" %in% all.names(body(get(
    "stat_rows_density_2d", envir = define_underscore
  ))))

  define_compact <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::stat_density2d, env = define_compact)
  adapt_layer_to_biplot(ggplot2::geom_density2d, env = define_compact)
  expect_true(exists(
    "stat_rows_density2d", envir = define_compact, inherits = FALSE
  ))
  expect_true(exists(
    "stat_cols_density2d", envir = define_compact, inherits = FALSE
  ))
  expect_true(exists(
    "geom_rows_density2d", envir = define_compact, inherits = FALSE
  ))
  expect_false(exists(
    "stat_rows_density_2d", envir = define_compact, inherits = FALSE
  ))

  string_underscore <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot("ggplot2::stat_density_2d", env = string_underscore)
  expect_true(exists(
    "stat_rows_density_2d", envir = string_underscore, inherits = FALSE
  ))

  string_compact <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot("ggplot2::stat_density2d", env = string_compact)
  expect_true(exists(
    "stat_rows_density2d", envir = string_compact, inherits = FALSE
  ))
})

test_that("density layer naming follows ggproto class for ggproto inputs", {
  define_object <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::StatDensity2d, env = define_object)
  expect_true(exists(
    "stat_rows_density2d", envir = define_object, inherits = FALSE
  ))
  expect_false(exists(
    "stat_rows_density_2d", envir = define_object, inherits = FALSE
  ))

  define_string <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot("ggplot2::StatDensity2d", env = define_string)
  expect_true(exists(
    "stat_cols_density2d", envir = define_string, inherits = FALSE
  ))
  expect_false(exists(
    "stat_cols_density_2d", envir = define_string, inherits = FALSE
  ))
})

test_that("adapt_layer_to_biplot(method = \"parse\") preserves density layer naming", {
  adapt_underscore <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(
    ggplot2::stat_density_2d,
    env = adapt_underscore,
    method = "parse"
  )
  adapt_layer_to_biplot(
    ggplot2::geom_density_2d,
    env = adapt_underscore,
    method = "parse"
  )
  expect_true(exists(
    "stat_rows_density_2d", envir = adapt_underscore, inherits = FALSE
  ))
  expect_true(exists(
    "geom_rows_density_2d", envir = adapt_underscore, inherits = FALSE
  ))

  adapt_compact <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(
    ggplot2::stat_density2d,
    env = adapt_compact,
    method = "parse"
  )
  expect_true(exists(
    "stat_rows_density2d", envir = adapt_compact, inherits = FALSE
  ))

  adapt_object <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(
    ggplot2::StatDensity2d,
    env = adapt_object,
    method = "parse"
  )
  expect_true(exists(
    "stat_cols_density2d", envir = adapt_object, inherits = FALSE
  ))
  expect_false(exists(
    "stat_cols_density_2d", envir = adapt_object, inherits = FALSE
  ))
})

test_that("namespaced ggproto input resolves constructors without attaching", {
  skip_if_not_installed("gggda")
  skip_if(
    ! "StatDelaunay" %in% getNamespaceExports("gggda"),
    "gggda does not export StatDelaunay"
  )
  skip_if(
    "package:gggda" %in% search(),
    "gggda is attached"
  )

  suppressWarnings(rm(
    list = c(
      "StatRowsDelaunay",
      "StatColsDelaunay",
      "stat_rows_delaunay",
      "stat_cols_delaunay"
    ),
    envir = .GlobalEnv
  ))

  adapt_layer_to_biplot(gggda::StatDelaunay, env = .GlobalEnv)

  expect_true(exists("stat_rows_delaunay", envir = .GlobalEnv))
  expect_true("engine" %in% names(formals(get(
    "stat_rows_delaunay",
    envir = .GlobalEnv
  ))))

  l_rows <- get("stat_rows_delaunay", envir = .GlobalEnv)()
  expect_true(inherits(l_rows$stat, "StatRowsDelaunay"))
})

test_that("geoms generated from a global-like caller resolve internal helpers", {
  suppressWarnings(rm(
    list = c("geom_rows_point", "geom_cols_point"),
    envir = .GlobalEnv
  ))

  caller <- rlang::new_environment(parent = globalenv())
  rlang::eval_bare(
    rlang::expr(adapt_layer_to_biplot(ggplot2::geom_point, env = globalenv())),
    env = caller
  )

  l_rows <- get("geom_rows_point", envir = .GlobalEnv)()
  expect_true(inherits(l_rows, "LayerInstance"))
  expect_true(inherits(l_rows$stat, "StatRows"))
})

test_that("namespaced geoms work from a global-like caller without attaching", {
  skip_if_not_installed("gggda")
  skip_if(
    "package:gggda" %in% search(),
    "gggda is attached"
  )

  suppressWarnings(rm(
    list = c("geom_rows_axis", "geom_cols_axis"),
    envir = .GlobalEnv
  ))

  caller <- rlang::new_environment(parent = globalenv())
  rlang::eval_bare(
    rlang::expr(adapt_layer_to_biplot(gggda::geom_axis, env = globalenv())),
    env = caller
  )

  l_rows <- suppressWarnings(get("geom_rows_axis", envir = .GlobalEnv)())
  expect_true(inherits(l_rows, "LayerInstance"))
  expect_true(inherits(l_rows$geom, "GeomAxis"))
})

test_that("constructors normalize match.arg() parameters", {
  skip_if_not_installed("ggrepel")

  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggrepel::geom_text_repel, env = env)

  body_text <- paste(
    deparse(body(get("geom_rows_text_repel", envir = env))),
    collapse = "\n"
  )
  expect_match(body_text, "direction = match.arg(direction)", fixed = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    get("geom_rows_text_repel", envir = env)(
      ggplot2::aes(label = rownames(mtcars)),
      direction = c("both", "y", "x")
    )
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("adapt_layer_to_biplot(method = \"parse\") rewrites geom layer calls", {
  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::geom_label, env = env, method = "parse")

  expect_true(exists("geom_rows_label", envir = env, inherits = FALSE))
  expect_true(exists("geom_cols_label", envir = env, inherits = FALSE))

  body_text <- paste(
    deparse(body(get("geom_rows_label", envir = env))),
    collapse = "\n"
  )
  expect_match(body_text, 'matrix_stat\\("rows",\\s+stat\\)')

  l_rows <- get("geom_rows_label", envir = env)()
  expect_s3_class(l_rows, "LayerInstance")
  expect_s3_class(l_rows$stat, "StatRows")
  l_cols <- get("geom_cols_label", envir = env)()
  expect_s3_class(l_cols, "LayerInstance")
  expect_s3_class(l_cols$stat, "StatCols")
})

test_that("adapt_layer_to_biplot(method = \"parse\") preserves layer-body transformations", {
  skip_if_not_installed("ggrepel")

  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggrepel::geom_text_repel, env = env, method = "parse")

  body_text <- paste(
    deparse(body(get("geom_rows_text_repel", envir = env))),
    collapse = "\n"
  )
  expect_match(body_text, "direction = match.arg(direction)", fixed = TRUE)
  expect_match(body_text, "to_unit(box.padding)", fixed = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    get("geom_rows_text_repel", envir = env)(
      ggplot2::aes(label = rownames(mtcars)),
      direction = c("both", "y", "x")
    )
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("adapt_layer_to_biplot(method = \"parse\") supports referential stats", {
  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(stat_projection, env = env, method = "parse")

  expect_true(exists("StatRowsProjection", envir = env, inherits = FALSE))
  expect_true(exists("stat_rows_projection", envir = env, inherits = FALSE))
  expect_true("ref_subset" %in% names(formals(get(
    "stat_rows_projection",
    envir = env
  ))))
  expect_true("ref_elements" %in% names(formals(get(
    "stat_rows_projection",
    envir = env
  ))))

  l_rows <- get("stat_rows_projection", envir = env)()
  expect_s3_class(l_rows, "LayerRef")
  expect_s3_class(l_rows$stat, "StatRowsProjection")
})

test_that("adapt_layer_to_biplot(method = \"parse\") rejects unsupported constructor bodies", {
  GeomBad <- ggplot2::ggproto(
    "GeomBad",
    ggplot2::Geom,
    required_aes = c("x", "y")
  )
  geom_bad <- function(...) NULL

  env <- rlang::new_environment(parent = globalenv())
  expect_error(
    adapt_layer_to_biplot(geom_bad, env = env, method = "parse"),
    "exactly one `layer\\(\\)` call"
  )
})

test_that("non-referential stats inherit setup_params from their parent", {
  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::stat_unique, env = env)

  rows_ggproto <- get("StatRowsUnique", envir = env)
  expect_true(is.function(rows_ggproto$setup_params))
  expect_identical(
    body(rows_ggproto$setup_params),
    body(ggplot2::StatUnique$setup_params)
  )

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    get("stat_rows_unique", envir = env)()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("generated label geoms resolve lifecycle helpers", {
  env <- rlang::new_environment(parent = globalenv())
  adapt_layer_to_biplot(ggplot2::geom_label, env = env)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    suppressWarnings(get("geom_rows_label", envir = env)(
      ggplot2::aes(label = rownames(mtcars))
    ))
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    geom_rows_label(ggplot2::aes(label = rownames(mtcars)))
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})

test_that("biplot layer arguments are validated", {
  env <- rlang::new_environment(parent = globalenv())

  expect_error(
    adapt_layer_to_biplot(ggplot2::geom_point, env = env, matrix = "points"),
    "`matrix`"
  )
  expect_error(
    adapt_layer_to_biplot(
      ggplot2::geom_point,
      env = env,
      referential = "yes"
    ),
    "`referential`"
  )
  expect_error(
    adapt_layer_to_biplot(ggplot2::geom_point, env = env, planar = 1),
    "`planar`"
  )
  expect_error(
    adapt_layer_to_biplot(ggplot2::geom_point, env = env, force = NA),
    "`force`"
  )
  expect_error(
    adapt_layer_to_biplot(ggplot2::geom_point, env = list()),
    "`env`"
  )
  expect_error(
    adapt_layer_to_biplot("ggplot2::geom_point::extra", env = env),
    "at most one `::`"
  )
})

test_that("adapt_layer_to_biplot() dispatches on method", {
  synth_direct <- rlang::new_environment(parent = globalenv())
  synth_wrapper <- rlang::new_environment(parent = globalenv())
  ordr:::adapt_layer_to_biplot_synth(ggplot2::geom_label, env = synth_direct)
  adapt_layer_to_biplot(ggplot2::geom_label, env = synth_wrapper)

  parse_direct <- rlang::new_environment(parent = globalenv())
  parse_wrapper <- rlang::new_environment(parent = globalenv())
  ordr:::adapt_layer_to_biplot_parse(ggplot2::geom_label, env = parse_direct)
  adapt_layer_to_biplot(
    ggplot2::geom_label,
    env = parse_wrapper,
    method = "parse"
  )

  expect_identical(
    deparse(body(get("geom_rows_label", envir = synth_direct))),
    deparse(body(get("geom_rows_label", envir = synth_wrapper)))
  )
  expect_identical(
    deparse(body(get("geom_rows_label", envir = parse_direct))),
    deparse(body(get("geom_rows_label", envir = parse_wrapper)))
  )
  expect_error(
    adapt_layer_to_biplot(
      ggplot2::geom_label,
      env = synth_wrapper,
      method = "unsupported"
    ),
    "should be one of"
  )
})

test_that("planar flag controls setup functions for stats", {
  df <- data.frame(
    .matrix = "rows",
    .element = "active",
    ..coord1 = 1:3,
    ..coord2 = 4:6,
    stringsAsFactors = FALSE
  )
  params <- list()

  for (method in c("synth", "parse")) {
    env <- rlang::new_environment(parent = globalenv())
    adapt_layer_to_biplot(
      stat_ellipse,
      env = env,
      planar = TRUE,
      method = method
    )
    ggproto_obj <- get("StatRowsEllipse", envir = env)
    expect_equal(
      ggproto_obj$setup_data(df, params),
      ordr:::setup_rows_xy_data(df, params)
    )

    env <- rlang::new_environment(parent = globalenv())
    adapt_layer_to_biplot(
      stat_ellipse,
      env = env,
      planar = FALSE,
      method = method
    )
    ggproto_obj <- get("StatRowsEllipse", envir = env)
    expect_equal(
      ggproto_obj$setup_data(df, params),
      ordr:::setup_rows_data(df, params)
    )
  }

  skip_if_not_installed("gggda")

  for (method in c("synth", "parse")) {
    env <- rlang::new_environment(parent = globalenv())
    adapt_layer_to_biplot(
      gggda::stat_voronoi,
      env = env,
      planar = NA,
      method = method
    )
    ggproto_obj <- get("StatRowsVoronoi", envir = env)
    expect_equal(
      ggproto_obj$setup_data(df, params),
      ordr:::setup_rows_data(df, params)
    )

    env <- rlang::new_environment(parent = globalenv())
    adapt_layer_to_biplot(
      stat_density2d,
      env = env,
      planar = NA,
      method = method
    )
    ggproto_obj <- get("StatRowsDensity2d", envir = env)
    expect_equal(
      ggproto_obj$setup_data(df, params),
      ordr:::setup_rows_xy_data(df, params)
    )
  }
})

test_that("referential flag adds extra_params to ggproto objects", {
  for (method in c("synth", "parse")) {
    env <- rlang::new_environment(parent = globalenv())
    adapt_layer_to_biplot(
      stat_ellipse,
      env = env,
      referential = TRUE,
      method = method
    )
    expect_true(exists("StatRowsEllipse", envir = env, inherits = FALSE))
    ggproto_obj <- get("StatRowsEllipse", envir = env)
    expect_true(all(
      c("ref_subset", "ref_elements") %in% ggproto_obj$extra_params
    ))
  }
})
