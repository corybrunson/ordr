#' @title Adapt `ggplot` Layers for `ggbiplot` Biplots
#'
#' @description `adapt_layer_to_biplot()` creates row and column versions of a
#'   ggplot2 layer (stat or geom) in the current environment. With
#'   `method = "synth"`, it synthesizes constructors from layer formals; with
#'   `method = "parse"`, it adapts the constructor's own `layer()` call,
#'   preserving argument transformations such as `match.arg()` and unit
#'   conversions.
#'
#' @param layer_spec A constructor function (e.g. `geom_point`), a ggproto
#'   object (e.g. `StatEllipse`), or the name of one (e.g. `gggda::stat_depth`).
#'   For an object in a package that may not be attached, use the namespaced
#'   accessor (e.g. `gggda::StatDelaunay`) so the package can be identified.
#' @param env Environment where the new objects will be defined. Defaults to the
#'   caller's environment.
#' @param matrix Character vector of matrix factors to generate layers for.
#'   Default `c("rows", "cols")`.
#' @param referential Logical; whether to make a referential layer (stat only).
#'   If `NA` (the default), inferred from the subclass returned by the input
#'   layer function.
#' @param planar Logical; whether to convert `..coord1` and `..coord2` to `x`
#'   and `y` during data setup; required only for stats that accept [ord_aes()].
#'   If `NA` (the default), inferred from `$required_aes`.
#' @param force Logical; whether to overwrite existing ggprotos and constructors
#'   in `env`. If `FALSE` (the default), they are untouched with a warning.
#' @param method Character; adaptation method. `"synth"` (default) synthesizes
#'   constructors from layer formals; `"parse"` rewrites the original
#'   constructor's `layer()` call.
#' @return `NULL`, invisibly. Defines ggproto objects (for stats) and
#'   constructor functions in `env` as side effects.
#' @details `method = "parse"` requires the original constructor to contain
#'   exactly one `layer()` call with named arguments. It rewrites that call's
#'   `stat` argument and, for referential stats, its `params` argument. Use it
#'   for constructors whose bodies contain transformations that cannot be
#'   reconstructed by `method = "synth"`.
#' @examples
#' # row and column versions of `geom_curve()`
#' adapt_layer_to_biplot(geom_curve)
#' geom_rows_curve
#' geom_cols_curve
#'
#' # row and column versions of `stat_binhex()`
#' adapt_layer_to_biplot(stat_binhex)
#' StatRowsBinhex
#' StatColsBinhex
#' stat_rows_binhex
#' stat_cols_binhex
#'
#' # preserve the constructor's `layer()` call
#' adapt_layer_to_biplot(ggplot2::geom_label, method = "parse")
#' geom_rows_label
#' geom_cols_label
#' 
#' @examplesIf rlang::is_installed("hexbin")
#' # use these layers in a biplot
#' ordinate(swiss, model = factanal, factors = 2L, scores = "regression") |> 
#'   ggbiplot() +
#'   coord_equal() +
#'   stat_rows_binhex(bins = 12, elements = "score") +
#'   geom_cols_curve(
#'     aes(alpha = uniqueness), xend = 0, yend = 0, curvature = .3,
#'     arrow = arrow(length = unit(.2, "cm"), ends = "first", type = "closed")
#'   ) +
#'   geom_cols_label(aes(label = name), vjust = "outward", size = 3, alpha = .8)
#' @seealso [biplot-stats] and [biplot-geoms]
#' @importFrom lifecycle deprecated
#' @export
adapt_layer_to_biplot <- function(
    layer_spec,
    env = rlang::caller_env(),
    matrix = c("rows", "cols"),
    referential = NA,
    planar = NA,
    force = FALSE,
    method = c("synth", "parse")
) {
  method <- match.arg(method)
  call_env <- rlang::caller_env()
  layer_expr <- rlang::enexpr(layer_spec)
  adapt_fun <- switch(
    method,
    synth = adapt_layer_to_biplot_synth,
    parse = adapt_layer_to_biplot_parse
  )
  adapt_fun(
    layer_spec = layer_spec,
    layer_expr = layer_expr,
    call_env = call_env,
    env = env,
    matrix = matrix,
    referential = referential,
    planar = planar,
    force = force
  )
}

adapt_layer_to_biplot_synth <- function(
    layer_spec,
    layer_expr = NULL,
    call_env = rlang::caller_env(),
    env = rlang::caller_env(),
    matrix = c("rows", "cols"),
    referential = NA,
    planar = NA,
    force = FALSE
) {
  if (is.null(layer_expr)) layer_expr <- rlang::enexpr(layer_spec)
  build_define_layers(
    layer_spec = layer_spec,
    layer_expr = layer_expr,
    env = env,
    call_env = call_env,
    matrix = matrix,
    referential = referential,
    planar = planar,
    force = force
  )
}

adapt_layer_to_biplot_parse <- function(
    layer_spec,
    layer_expr = NULL,
    call_env = rlang::caller_env(),
    env = rlang::caller_env(),
    matrix = c("rows", "cols"),
    referential = NA,
    planar = NA,
    force = FALSE
) {
  if (is.null(layer_expr)) layer_expr <- rlang::enexpr(layer_spec)
  build_adapt_layers(
    layer_spec = layer_spec,
    layer_expr = layer_expr,
    env = env,
    call_env = call_env,
    matrix = matrix,
    referential = referential,
    planar = planar,
    force = force
  )
}

# collect `match.arg()` arguments from an expression
match_arg_names <- function(expr) {
  if (is.call(expr)) {
    head <- expr[[1L]]
    is_match_arg <- identical(head, quote(match.arg)) || (
      rlang::is_call(head, "::") &&
        identical(head[[3L]], quote(match.arg))
    ) || (
      rlang::is_call(head, ":::") &&
        identical(head[[3L]], quote(match.arg))
    )
    if (
      is_match_arg &&
        length(expr) >= 2L &&
        rlang::is_symbol(expr[[2L]])
    ) {
      return(rlang::as_string(expr[[2L]]))
    }
    out <- unlist(lapply(as.list(expr)[-1L], match_arg_names))
    return(if (is.null(out)) character() else unique(out))
  }
  if (is.pairlist(expr)) {
    out <- unlist(lapply(as.list(expr), match_arg_names))
    return(if (is.null(out)) character() else unique(out))
  }
  character()
}

# validate a length-one logical argument
validate_flag <- function(name, value, allow_na = FALSE) {
  if (! is.logical(value) || length(value) != 1L) {
    rlang::abort(paste0("`", name, "` must be a length-one logical vector."))
  }
  if (! allow_na && is.na(value)) {
    rlang::abort(paste0("`", name, "` must be `TRUE` or `FALSE`."))
  }
  value
}

# parse a layer specification into package, constructor, and ggproto
parse_layer_spec <- function(layer_spec, layer_expr, call_env) {
  expr_pkg <- if (rlang::is_call(layer_expr, "::")) {
    rlang::as_string(layer_expr[[2L]])
  } else {
    NULL
  }
  if (is.character(layer_spec)) {
    name_pkg <- layer_spec
  } else if (is.function(layer_spec)) {
    name_pkg <- if (rlang::is_symbol(layer_expr)) {
      rlang::as_string(layer_expr)
    } else {
      rlang::as_label(layer_expr)
    }
  } else if (inherits(layer_spec, "Stat") || inherits(layer_spec, "Geom")) {
    type <- if (inherits(layer_spec, "Stat")) "stat" else "geom"
    snake <- ggplot2:::snake_class(layer_spec)
    name_pkg <- paste0(type, "_", sub(paste0("^", type, "_"), "", snake))
  } else {
    rlang::abort(paste0(
      "Unsupported `layer_spec` type. ",
      "Provide a string, function, or ggproto."
    ))
  }
  if (grepl("::", name_pkg, fixed = TRUE)) {
    parts <- strsplit(name_pkg, "::", fixed = TRUE)[[1L]]
    if (length(parts) != 2L || any(! nzchar(parts))) {
      rlang::abort("`layer_spec` must contain at most one `::`.")
    }
    spec <- list(pkg = parts[1L], name = parts[2L])
  } else {
    spec <- list(pkg = expr_pkg, name = name_pkg)
  }
  pkg <- spec$pkg
  lname <- spec$name
  if (grepl("^(Stat|Geom)", lname)) {
    ggproto_class <- if (is.null(pkg)) {
      get(lname, envir = call_env)
    } else {
      getFromNamespace(lname, pkg)
    }
    if (! identical(class(ggproto_class)[1L], lname)) {
      rlang::abort(paste0(
        "Layer ggproto class `", class(ggproto_class)[1L],
        "` does not match constructor `", lname, "`."
      ))
    }
    lname <- ggplot2:::snake_class(ggproto_class)
    layer_spec <- ggproto_class
  }
  type <- if (grepl("^stat_", lname)) {
    "stat"
  } else if (grepl("^geom_", lname)) {
    "geom"
  } else {
    rlang::abort("Constructor name must start with 'stat_' or 'geom_'.")
  }
  base_name <- sub(paste0("^", type, "_"), "", lname)
  layer_fun <- if (is.function(layer_spec)) {
    layer_spec
  } else if (is.null(pkg)) {
    get(lname, envir = call_env)
  } else {
    getFromNamespace(lname, pkg)
  }
  ggproto_class_name <- paste0(
    if (type == "stat") "Stat" else "Geom",
    ggplot2:::camelize(base_name, first = TRUE)
  )
  ggproto_class <- if (
    inherits(layer_spec, "Stat") ||
    inherits(layer_spec, "Geom")
  ) {
    layer_spec
  } else if (is.null(pkg)) {
    get(ggproto_class_name, envir = call_env)
  } else {
    getFromNamespace(ggproto_class_name, pkg)
  }
  prefix <- if (type == "stat") "Stat" else "Geom"
  ggproto_class_label <- class(ggproto_class)[1L]
  if (! startsWith(ggproto_class_label, prefix)) {
    rlang::abort(paste0(
      "Layer ggproto class `", ggproto_class_label,
      "` does not match constructor `", lname, "`."
    ))
  }
  expected_suffix <- sub(paste0("^", prefix), "", ggproto_class_label)
  provided_suffix <- ggplot2:::camelize(base_name, first = TRUE)
  if (! identical(expected_suffix, provided_suffix)) {
    rlang::abort(paste0(
      "Layer ggproto class `", ggproto_class_label,
      "` does not match constructor `", lname, "`."
    ))
  }
  list(
    pkg = pkg,
    name = lname,
    type = type,
    base_name = sub(paste0("^", type, "_"), "", lname),
    layer_fun = layer_fun,
    ggproto_class = ggproto_class
  )
}

# infer referential and planar flags
infer_biplot_flags <- function(layer_fun, ggproto_class, referential, planar) {
  referential <- validate_flag("referential", referential, allow_na = TRUE)
  planar <- validate_flag("planar", planar, allow_na = TRUE)
  # determine planarity if not user-specified
  if (is.na(planar)) {
    # require xy conversion if original rejects `..coord[0-9]+` aesthetics
    req_coord <- grepl("\\.\\.coord[0-9]+", ggproto_class$required_aes)
    planar <- ! any(req_coord)
  }

  # determine referentiality if not user-specified
  if (is.na(referential)) {
    # require referent parameter if constructor has 'LayerRef' class
    referential <- "LayerRef" %in% all.names(body(layer_fun))
    if (! referential) {
      layer_fun_inst <- tryCatch(layer_fun(), error = function(e) NULL)
      referential <- inherits(layer_fun_inst, "LayerRef")
    }
  }
  list(referential = referential, planar = planar)
}

# name a rows or cols stat class
stat_class_name <- function(mat, base_name) {
  ggplot2:::camelize(
    paste0("stat_", mat, "_", base_name),
    first = TRUE
  )
}

# build a rows or cols stat ggproto
make_stat_ggproto <- function(
  ggproto_class,
  new_class_name,
  mat,
  planar,
  referential
) {
  setup_fun <- if (planar) {
    if (mat == "rows") setup_rows_xy_data else setup_cols_xy_data
  } else {
    if (mat == "rows") setup_rows_data else setup_cols_data
  }
  members <- list(
    setup_data = setup_fun,
    compute_group = ord_formals(ggproto_class, "compute_group")
  )
  if (referential) {
    members$setup_params <- setup_referent_params
    members$extra_params <- c(
      ggproto_class$extra_params,
      "ref_subset",
      "ref_elements"
    )
  }
  do.call(ggproto, c(list(new_class_name, ggproto_class), members))
}

# add referential formals to constructor arguments
add_referential_formals <- function(orig_formals, type, referential) {
  if (! "..." %in% names(orig_formals)) {
    orig_formals <- c(orig_formals, alist(... = ))
  }
  # add referential-specific formals for stats
  if (type == "stat" && isTRUE(referential)) {
    all_ref_formals <- alist(ref_subset = NULL, ref_elements = "active")
    extra_ref_formals <-
      all_ref_formals[setdiff(names(all_ref_formals), names(orig_formals))]
    # "..." is guaranteed by the ellipsis-preservation block above
    dots_pos <- match("...", names(orig_formals))
    orig_list <- as.list(orig_formals)
    orig_list <- append(orig_list, extra_ref_formals, after = dots_pos - 1L)
    orig_formals <- as.pairlist(orig_list)
  }
  orig_formals
}

# assign a generated object to the target environment
assign_biplot_object <- function(name, value, env, force, kind) {
  obj_exists <- rlang::env_has(env, name)[[1L]]
  if (obj_exists && ! force) {
    rlang::warn(paste0(
      kind, " '", name,
      "' already exists; skipping (use `force = TRUE` to overwrite)."
    ))
    return(invisible(FALSE))
  }
  if (obj_exists && force) {
    rlang::warn(paste0(
      "Overwriting existing ", kind, " '", name,
      "' (`force = TRUE`)."
    ))
  }
  rlang::env_bind(env, !! name := value)
  invisible(TRUE)
}

wrap_layer_ref <- function(expr, referential) {
  if (! isTRUE(referential)) return(expr)
  rlang::expr({
    .ordr_layer <- !! expr
    if (! inherits(.ordr_layer, "LayerRef")) {
      class(.ordr_layer) <- c("LayerRef", class(.ordr_layer))
    }
    .ordr_layer
  })
}

biplot_root_args <- c(
  "mapping", "data",
  "geom", "stat", "position",
  "show.legend", "inherit.aes", "..."
)

# build the params call for a generated constructor
make_params_call <- function(layer_fun, orig_formals) {
  # determine which arguments should be forwarded to layer params
  # (excluding root args)
  param_names <- setdiff(names(orig_formals), biplot_root_args)
  param_syms <- rlang::set_names(rlang::syms(param_names), param_names)
  matched_args <- intersect(match_arg_names(body(layer_fun)), param_names)
  param_exprs <- lapply(param_syms, function(sym) {
    if (rlang::as_string(sym) %in% matched_args) {
      rlang::expr(match.arg(!! sym))
    } else {
      sym
    }
  })
  # build call: rlang::list2(arg1 = arg1, ..., ...)
  rlang::expr(rlang::list2(!!! param_exprs, ...))
}

# build the body of a generated constructor
build_define_body <- function(
  mat,
  layer_fun,
  type,
  base_name,
  new_class_name,
  referential,
  orig_formals
) {
  params_call <- make_params_call(layer_fun, orig_formals)

  if (type == "stat") {
    # core layer call for stats
    core_call <- rlang::expr(
      layer(
        mapping = mapping,
        data = data,
        geom = geom,
        stat = !! rlang::sym(new_class_name),
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = !! params_call
      )
    )
    wrap_layer_ref(core_call, referential)
  } else {
    # core layer call for geoms (no referential handling)
    rlang::expr(
      layer(
        mapping = mapping,
        data = data,
        geom = !! base_name,
        stat = matrix_stat(!! mat, stat),
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = !! params_call
      )
    )
  }
}

# detect a layer call in an expression
is_layer_call <- function(expr) {
  if (! rlang::is_call(expr)) return(FALSE)
  head <- expr[[1L]]
  identical(head, quote(layer)) || (
    (rlang::is_call(head, "::") || rlang::is_call(head, ":::")) &&
      identical(head[[3L]], quote(layer))
  )
}

# adapt a layer call to a biplot statistic
adapt_layer_call <- function(
  call,
  mat,
  type,
  new_class_name,
  referential
) {
  parts <- as.list(call)
  nms <- names(parts)
  if (is.null(nms)) nms <- rep("", length(parts))
  if (any(! nzchar(nms[-1L]))) {
    rlang::abort("`layer()` must be called with named arguments.")
  }
  stat_pos <- which(nms == "stat")
  if (length(stat_pos) > 1L) {
    rlang::abort("`layer()` must not have duplicate `stat` arguments.")
  }
  new_stat <- if (type == "stat") {
    rlang::sym(new_class_name)
  } else {
    rlang::expr(matrix_stat(!! mat, stat))
  }
  if (length(stat_pos) == 1L) {
    parts[[stat_pos]] <- new_stat
  } else {
    parts <- c(parts, list(stat = new_stat))
  }
  if (type == "stat" && isTRUE(referential)) {
    params_pos <- which(nms == "params")
    if (length(params_pos) > 1L) {
      rlang::abort("`layer()` must not have duplicate `params` arguments.")
    }
    if (length(params_pos) == 1L) {
      params_call <- parts[[params_pos]]
      if (! rlang::is_call(params_call)) {
        rlang::abort("`params` must be a `list()` or `list2()` call.")
      }
      params_head <- rlang::call_name(params_call)
      if (! params_head %in% c("list", "list2")) {
        rlang::abort("`params` must be a `list()` or `list2()` call.")
      }
      params_parts <- as.list(params_call)
      params_nms <- names(params_parts)
      if (is.null(params_nms)) {
        params_nms <- rep("", length(params_parts))
      }
      if (! "ref_subset" %in% params_nms) {
        params_parts <- c(
          params_parts,
          list(ref_subset = quote(ref_subset))
        )
      }
      if (! "ref_elements" %in% params_nms) {
        params_parts <- c(
          params_parts,
          list(ref_elements = quote(ref_elements))
        )
      }
      parts[[params_pos]] <- as.call(params_parts)
    } else {
      parts <- c(parts, list(params = rlang::expr(list(
        ref_subset = ref_subset,
        ref_elements = ref_elements
      ))))
    }
  }
  as.call(parts)
}

# adapt the layer call in a constructor body
adapt_body <- function(expr, mat, type, new_class_name, referential) {
  layer_count <- 0L
  # walk the constructor body and adapt its layer call
  walk <- function(x) {
    if (! is.call(x)) return(x)
    if (is_layer_call(x)) {
      layer_count <<- layer_count + 1L
      if (layer_count > 1L) {
        rlang::abort(
          "Constructor body must contain exactly one `layer()` call."
        )
      }
      return(adapt_layer_call(
        x,
        mat = mat,
        type = type,
        new_class_name = new_class_name,
        referential = referential
      ))
    }
    parts <- as.list(x)
    if (length(parts) > 1L) {
      for (i in seq.int(2L, length(parts))) {
        parts[[i]] <- walk(parts[[i]])
      }
    }
    as.call(parts)
  }
  expr <- walk(expr)
  if (layer_count == 0L) {
    rlang::abort("Constructor body must contain exactly one `layer()` call.")
  }
  expr
}

# build the body of an adapted constructor
build_adapt_body <- function(
  mat,
  layer_fun,
  type,
  base_name,
  new_class_name,
  referential,
  orig_formals
) {
  body_expr <- rlang::zap_srcref(body(layer_fun))
  body_expr <- adapt_body(
    body_expr,
    mat = mat,
    type = type,
    new_class_name = new_class_name,
    referential = referential
  )
  if (type == "stat") {
    has_layer_ref <- "LayerRef" %in% all.names(body_expr)
    if (identical(referential, FALSE) && has_layer_ref) {
      rlang::abort(paste0(
        "Constructor body already creates `LayerRef`; ",
        "use `referential = TRUE` or `method = \"synth\"`."
      ))
    }
    if (isTRUE(referential) && ! has_layer_ref) {
      body_expr <- wrap_layer_ref(body_expr, TRUE)
    }
  }
  body_expr
}

# create a builder for biplot layer definitions
make_biplot_layer_builder <- function(build_body, fun_env_source) {
  build_body <- rlang::as_function(build_body)
  fun_env_source <- match.arg(fun_env_source, c("builder", "layer"))
  function(
    layer_spec,
    layer_expr,
    env,
    call_env,
    matrix,
    referential,
    planar,
    force
  ) {
    if (! is.environment(env)) {
      rlang::abort("`env` must be an environment.")
    }
    if (
      ! is.character(matrix) ||
        length(matrix) < 1L ||
        anyNA(matrix) ||
        ! all(matrix %in% c("rows", "cols"))
    ) {
      rlang::abort("`matrix` must contain `\"rows\"`, `\"cols\"`, or both.")
    }
    force <- validate_flag("force", force)
    spec <- parse_layer_spec(layer_spec, layer_expr, call_env)

    flags <- infer_biplot_flags(
      spec$layer_fun,
      spec$ggproto_class,
      referential,
      planar
    )
    # for requested matrix factor(s)
    for (mat in matrix) {
      new_class_name <- if (spec$type == "stat") {
        stat_class_name(mat, spec$base_name)
      } else {
        NULL
      }
      # define ggproto for stat layers
      if (spec$type == "stat") {
        assign_biplot_object(
          new_class_name,
          make_stat_ggproto(
            spec$ggproto_class,
            new_class_name,
            mat,
            flags$planar,
            flags$referential
          ),
          env,
          force,
          "ggproto object"
        )
      }
      # constructor function name
      fun_name <- paste0(spec$type, "_", mat, "_", spec$base_name)

      # function formals (copy from original)
      orig_formals <- add_referential_formals(
        formals(spec$layer_fun),
        spec$type,
        flags$referential
      )
      body_expr <- build_body(
        mat = mat,
        layer_fun = spec$layer_fun,
        type = spec$type,
        base_name = spec$base_name,
        new_class_name = new_class_name,
        referential = flags$referential,
        orig_formals = orig_formals
      )
      parent_env <- if (fun_env_source == "builder") {
        rlang::fn_env(build_body)
      } else {
        rlang::fn_env(spec$layer_fun)
      }
      fun_env <- rlang::new_environment(parent = parent_env)
      if (spec$type == "stat") {
        rlang::env_bind(
          fun_env,
          !! new_class_name := rlang::env_get(env, new_class_name)
        )
      } else if (fun_env_source == "layer") {
        rlang::env_bind(fun_env, matrix_stat = matrix_stat)
      }
      # assemble function
      new_fun <- rlang::new_function(orig_formals, body_expr, env = fun_env)
      # assign to target environment
      assign_biplot_object(fun_name, new_fun, env, force, "constructor")
    }
    invisible(NULL)
  }
}

# build row and column layers by generating constructor bodies
build_define_layers <- make_biplot_layer_builder(
  build_define_body,
  fun_env_source = "builder"
)

# build row and column layers by adapting constructor bodies
build_adapt_layers <- make_biplot_layer_builder(
  build_adapt_body,
  fun_env_source = "layer"
)
