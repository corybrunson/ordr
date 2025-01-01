## This script builds `*_rows_*()` and `*_cols_*()` biplot layers.
## To update them, source this script as follows:
## source(here::here("build-pre/build-layers.r"))

library(devtools)
library(stringr)

# standard prefix for built layer files
build_prefix <- "zzz-biplot-"

## 0. Remove previous builds.
Sys.sleep(.5)

# remove previous built layer files
built_files <-
  list.files(here::here("R"), str_c(build_prefix, "(stat|geom)s.r"))
file.remove(here::here("R", built_files))
# re-document package without built files and load remaining objects
document()
load_all()

## 1. Prepare function/documentation generators.
## Resource: <https://blog.r-hub.io/2020/02/10/code-generation/>

# format values passed to arguments so as to be formatted for gluing
format_formal <- function(x) {
  str_replace(
    format(enquote(rlang::enquo(x))),
    "^base::quote\\(\\~(.*)\\)$", "\\1"
  )
}

# arguments to include in the root of each `layer()` call (not in `params = `)
root_args <- c(
  "mapping", "data", "stat", "geom", "position",
  "show.legend", "inherit.aes"
)

# concatenate arguments and values in roxygen2 markdown format
arg_c <- function(x, y, indent = 0L, end = FALSE) {
  stopifnot(length(x) == length(y))
  y_seq <- seq(if (end && length(y) > 1L) {
    length(y) - 1L
  } else if (end && length(y) == 1L) {
    c()
  } else {
    length(y)
  })
  y[y_seq] <- str_c(y[y_seq], ",")
  ind <- str_c(c("\n", rep(" ", indent)), collapse = "")
  str_c(x, y, sep = " = ", collapse = ind)
}

# parameters transformed by default in `layer()` calls
# -+- should eventually extract these automatically from layers -+-
param_trans <- c(
  box.padding = format_formal(to_unit(box.padding)),
  point.padding = format_formal(to_unit(point.padding)),
  min.segment.length = format_formal(to_unit(min.segment.length)),
  direction = format_formal(match.arg(direction))
)

# necessary internal functions not exported from their home packages
# -+- currently only put in geoms file; need to distinguish -+-
get_from <- c(
  # `geom_vector()`
  compute_just = "ggplot2",
  # `geom_text_repel()`
  to_unit = "ggrepel"
)

# convert `nudge_x/nudge_y` parameters to position parameter
nudges_to_position <- str_c(
  "  if (! missing(nudge_x) || ! missing(nudge_y)) {{\n",
  "    if (! missing(position)) {{\n",
  "      stop(\"Specify either `position` or `nudge_x`/`nudge_y`\", ",
  "call. = FALSE)\n",
  "    }}\n",
  "    position <- position_nudge(nudge_x, nudge_y)\n",
  "  }}\n\n"
)

# function to generate layer functions, ggproto objects, and their documentation
build_biplot_layer <- function(
    layer_name, .matrix, proto = TRUE, ref = FALSE, xy = FALSE, file = ""
) {
  .matrix <- match_factor(.matrix)
  
  # verify uniplot layer and derive biplot layer name
  #layer_name <- rlang::enexpr(layer)
  #layer <- get(layer_name)
  layer_pkg <- if (str_detect(layer_name, "^.*::")) {
    str_remove(layer_name, "::.*$")
  } else {
    NA
  }
  layer_name <- str_remove(layer_name, "^.*::")
  layer <- if (is.na(layer_pkg)) {
    get(layer_name)
  } else {
    utils::getFromNamespace(layer_name, layer_pkg)
  }
  if (! str_detect(layer_name, "^(stat|geom)\\_")) {
    stop("`", layer_name, "` is not a recognized layer.")
  }
  type <- str_extract(layer_name, "^(stat|geom)")
  name <- str_remove(layer_name, "^(stat|geom)\\_")
  
  # new ggproto objects are used for stats but not for geoms
  biplot_layer_name <- glue::glue("{type}_{.matrix}_{name}")
  ggproto_name <- switch(
    type,
    stat = ggplot2:::camelize(biplot_layer_name, first = TRUE),
    geom = ggplot2:::camelize(layer_name, first = TRUE)
  )
  ggparent_name <- ggplot2:::camelize(layer_name, first = TRUE)
  
  # get uniplot formals (and insert any additional biplot formals)
  # -+- extract this into a function that can handle `...` -+-
  layer_formals <- formals(layer)
  layer_formals <- unlist(lapply(layer_formals, format_formal))
  layer_args <- names(layer_formals)
  layer_vals <- unname(layer_formals)
  stopifnot(length(layer_args) == length(layer_vals))
  # do "..." separately
  stopifnot(layer_vals[match("...", layer_args)] == "")
  seq1 <- seq(match("...", layer_args) - 1L)
  layer_args1 <- layer_args[seq1]
  layer_vals1 <- layer_vals[seq1]
  stopifnot(length(layer_args1) > 0L)
  layer2 <- length(layer_args) > length(layer_args1) + 1L
  if (layer2) {
    seq2 <- seq(match("...", layer_args) + 1L, length(layer_args))
    layer_args2 <- layer_args[seq2]
    layer_vals2 <- layer_vals[seq2]
  }
  
  # define biplot layer root parameters
  root_vals <- root_args
  root_vals[[match("stat", root_args)]] <- switch(
    type,
    stat = ggproto_name,
    geom = glue::glue("{.matrix}_stat(stat)")
  )
  root_vals[[match("geom", root_args)]] <- switch(
    type,
    stat = "geom",
    geom = ggproto_name
  )
  
  # detect `nudge_x` and `nudge_y` parameters
  nudge_args <- any(c("nudge_x", "nudge_y") %in% layer_args)
  
  # define biplot layer internal parameters
  param_args <- setdiff(layer_args, c(root_args, "...", "nudge_x", "nudge_y"))
  # make any prespecified syntactic parameter transformations
  # -+- need to also specify package or layer to avoid ambiguity -+-
  param_vals <- param_args
  param_match <- intersect(param_args, names(param_trans))
  if (length(param_match) > 0L)
    param_vals[match(param_match, param_vals)] <-
    unname(param_trans[param_match])
  
  # define ggproto object
  # TODO: Incorporate `ensure_cartesian_polar()` if necessary where appropriate.
  if_xy <- if (xy) "_xy" else ""
  proto_def <- if (! proto) "" else switch(
    type,
    stat = glue::glue(
      "#' @rdname ordr-ggproto\n",
      "#' @format NULL\n",
      "#' @usage NULL\n",
      "#' @export\n",
      "{ggproto_name} <- ggproto(\n",
      "  \"{ggproto_name}\", {ggparent_name},\n",
      "  \n",
      if (ref) paste0(
        "  extra_params = c({ggparent_name}$extra_params, ",
        "\"ref_subset\", \"ref_elements\"),\n  \n"
      ) else "",
      if (ref) "  setup_params = setup_referent_params,\n  \n" else "",
      "  setup_data = setup_{.matrix}{if_xy}_data,\n  \n",
      "  compute_group = ord_formals({ggparent_name}, \"compute_group\")\n",
      ")\n",
      "\n\n",
    ),
    geom = ""
  )
  
  # write function
  layer_def <- glue::glue(
    "#' @rdname biplot-{type}s\n",
    "#' @export\n",
    "{biplot_layer_name} <- function(\n",
    "  ",
    arg_c(layer_args1, layer_vals1, 2L),
    if (ref) "\n  ref_subset = NULL, ref_elements = \"active\"," else "",
    "\n  ...",
    if (! layer2) "" else {
      str_c(",\n  ", arg_c(layer_args2, layer_vals2, 2L, end = TRUE))
    },
    "\n",
    ") {{\n",
    if (nudge_args) nudges_to_position else "",
    if (ref) "  LayerRef <- layer(\n" else "  layer(\n",
    "    ",
    arg_c(root_args, root_vals, 4L),
    "\n",
    "    params = list(\n",
    if (length(param_args) == 0L) "" else "      ",
    if (length(param_args) == 0L) "" else arg_c(param_args, param_vals, 6L),
    if (length(param_args) == 0L) "" else "\n",
    if (ref) "      ref_subset = ref_subset,\n" else "",
    if (ref) "      ref_elements = ref_elements,\n" else "",
    if ("na.rm" %in% param_args) "" else "      na.rm = FALSE,\n",
    "      ...\n",
    "    )\n",
    "  )\n",
    if (ref) "  class(LayerRef) <- c(\"LayerRef\", class(LayerRef))\n" else "",
    if (ref) "  LayerRef\n" else "",
    "}}\n"
  )
  
  # write to file
  cat("\n", proto_def, layer_def, "\n", file = file, sep = "", append = TRUE)
}

## 2. Search code for layers to generalize to rows/cols.
Sys.sleep(.5)

# ggplot2 & other (non-ordr) extension layers to adapt to biplot layers
orig_layers <- c(
  "ggplot2::stat_density_2d", "ggplot2::stat_density_2d_filled",
  "ggplot2::stat_ellipse",
  "ggplot2::geom_point", "ggplot2::geom_path", "ggplot2::geom_polygon",
  "ggplot2::geom_density_2d", "ggplot2::geom_density_2d_filled",
  "ggplot2::geom_text", "ggplot2::geom_label",
  "ggrepel::geom_text_repel", "ggrepel::geom_label_repel"
)
# ordr layers to not adapt to biplot layers
omit_layers <- c(
  "geom_origin", "geom_unit_circle",
  "stat_referent"
)

# layers that use reference data (necessarily 2 coordinates only)
ref_layers <- c(
  "stat_projection",
  "stat_rule"
)
# layers that require restriction to 2 coordinates (without package recoverers)
xy_layers <- c(
  "stat_density_2d", "stat_density_2d_filled",
  "stat_ellipse",
  "stat_scale",
  "stat_center", "stat_star",
  ref_layers
)

# all files with stat or geom definitions
layer_files <- list.files(here::here("R/"), "^(stat|geom)-.*\\.(r|R)")
# detect all uniplot layer definitions
layer_defs <- unlist(lapply(layer_files, function(f) {
  rl <- readLines(here::here("R", f))
  rl <- str_subset(rl, "^(stat|geom)\\_[a-z0-9\\_]+ <- ")
  rl <- str_subset(rl, "^(stat|geom)\\_(rows|cols)", negate = TRUE)
  str_extract(rl, "^(stat|geom)\\_[a-z0-9\\_]+")
}))
# layers to adapt to biplot layers
adapt_layers <- c(
  orig_layers,
  setdiff(layer_defs, omit_layers)
)

# custom (done) ggproto objects
# -+- should be only `Stat*Scale`; eventually automate? -+-
done_protos <- unlist(lapply(layer_files, function(f) {
  rl <- readLines(here::here("R", f))
  rl <- str_subset(rl, "^(Stat|Geom)[A-Za-z0-9]+ <- ")
  rl <- str_subset(rl, "^(Stat|Geom)(Rows|Cols)[A-Z]")
  str_extract(rl, "^(Stat|Geom)[A-Za-z0-9]+")
}))

## 3. Run generators on manual and found lists of layers.
Sys.sleep(.5)

# find layer-specific examples
ex_files <- list.files(here::here("inst/examples/"), "(stat|geom)-.*\\.(r|R)")
# restrict to examples without home documentation files
ex_match <- sapply(
  str_replace_all(str_remove(orig_layers, "^(.*::|)"), "\\_", "-"),
  str_match,
  string = ex_files
)
ex_incl <- ex_files[apply(! is.na(ex_match), 1L, any)]

# find layers from ggplot2 extensions to import/export
port_layers <- str_subset(orig_layers, "^ggplot2::", negate = TRUE)
port_pkgs <- str_extract(port_layers, "^[^:]+")
port_protos <-
  ggplot2:::camelize(str_remove(port_layers, "^[^:]+::"), first = TRUE)

# warning (80-character lines)
adapt_warn <- glue::glue(
  "# --------------------------------------",
  "----------------------------------------\n",
  "# Generated by 'build-pre/build.r': do not edit by hand.\n",
  "# --------------------------------------",
  "----------------------------------------\n\n\n",
)

for (type in c("stat", "geom")) {
  
  # file path
  adapt_file <- here::here(glue::glue("R/{build_prefix}{type}s.r"))
  
  # title paragraph
  adapt_title <- glue::glue(
    "#' @title Convenience {type}s for row and column matrix factors\n",
    "#' \n\n"
  )
  
  # description paragraph
  adapt_descr <- switch(
    type,
    stat = glue::glue(
      "#' @description These statistical transformations (stats) adapt\n",
      "#'   conventional **ggplot2** stats to one or the other matrix factor\n",
      "#'   of a tbl_ord, in lieu of [stat_rows()] or [stat_cols()]. They\n",
      "#'   accept the same parameters as their corresponding conventional\n",
      "#'   stats.\n",
      "#' \n\n"
    ),
    geom = glue::glue(
      "#' @description These geometric element layers (geoms) pair\n",
      "#'   conventional **ggplot2** geoms with [stat_rows()] or\n",
      "#'   [stat_cols()] in order to render elements for one or the other\n",
      "#'   matrix factor of a tbl_ord. They understand the same aesthetics\n",
      "#'   as their corresponding conventional geoms.\n",
      "#' \n\n"
    )
  )
  
  # standard roxygen2 tags
  adapt_roxygen <- glue::glue(
    "#' @name biplot-{type}s\n",
    "#' @template return-layer\n",
    "#' @family biplot layers\n",
    "#' @include utils.r\n",
    "#' @import ggplot2\n",
    "\n"
  )
  
  # derived imports
  wh_port <- str_which(port_layers, type)
  import_str <- sapply(unique(port_pkgs[wh_port]), function(pkg) {
    wh_pkg <- which(port_pkgs[wh_port] == pkg)
    str_c(
      "#' @importFrom ", pkg, "\n",
      str_c(
        "#'   ",
        c(
          port_protos[wh_port][wh_pkg],
          str_remove(port_layers[wh_port], "^[^:]+::")
        ),
        "\n",
        collapse = ""
      ),
      "\n"
    )
  })
  adapt_imports <- glue::glue(import_str)
  # derived exports
  adapt_exports <- if (length(wh_port) == 0L) "" else glue::glue(
    "\n\n",
    str_c("#' @export\n", port_layers[wh_port], "\n", collapse = ""),
    "\n"
  )
  
  # standard parameter inheritances
  adapt_inherits1 <- glue::glue(
    "#' @inheritParams ggplot2::layer\n",
    #if (type == "geom") "#' @template param-matrix\n" else "",
    "#' @template param-{type}\n",
    if (type == "stat") "#' @template biplot-ord-aes\n" else "",
    if (type == "stat") "#' @inheritParams stat_rows\n" else "",
    if (type == "stat") "#' @template param-referent\n" else "",
    "\n"
  )
  
  # derived parameter inheritances
  adapt_inherits2 <- glue::glue(
    str_c(
      str_c(
        "#' @inheritParams ",
        str_subset(adapt_layers, glue::glue("^(.*::|){type}")),
        "\n",
        collapse = ""
      ),
      "\n"
    )
  )
  
  # examples
  ex_type <- str_subset(ex_incl, glue::glue("{type}\\-"))
  adapt_examples <- if (length(ex_type) == 0L) "" else glue::glue(
    str_c(
      str_c(
        "#' @example inst/examples/",
        ex_type,
        "\n",
        collapse = ""
      ),
      "\n"
    )
  )
  
  # conclude roxygen2 tags
  adapt_fin <- glue::glue(
    "NULL\n\n"
  )
  
  # manual imports/exports
  gets_from_namespaces <- if (type == "stat" || length(get_from) == 0L) {
    ""
  } else {
    glue::glue(
      "\n\n",
      # taken care of by `@include utils.r`
      #"#' @importFrom utils getFromNamespace\n",
      str_c(
        names(get_from),
        " <- getFromNamespace(\"",
        names(get_from),
        "\", \"", unname(get_from), "\")\n",
        collapse = ""
      ),
      "\n"
    )
  }
  
  # write header
  cat(
    adapt_warn,
    adapt_title, adapt_descr,
    adapt_roxygen, adapt_imports,
    adapt_inherits1, adapt_inherits2,
    adapt_examples, adapt_fin,
    adapt_exports,
    gets_from_namespaces,
    file = adapt_file, sep = "", append = FALSE
  )
  
  # write functions with documentation
  write_layers <- str_subset(adapt_layers, glue::glue("^([^:]+::|){type}\\_"))
  for (write_layer in write_layers) for (.matrix in c("rows", "cols")) {
    
    # determine if ggproto object is already defined
    done_proto <-
      ggplot2:::camelize(str_remove(write_layer, "^.*::"), first = TRUE) %in%
      done_protos
    
    # build layer!!!
    build_biplot_layer(
      write_layer,
      .matrix,
      proto = ! done_proto,
      ref = str_remove(write_layer, "^.*::") %in% ref_layers,
      xy = str_remove(write_layer, "^.*::") %in% xy_layers,
      file = adapt_file
    )
  }
}

## 4. Update documentation with generated layers.
Sys.sleep(.5)

# update documentation
document()
