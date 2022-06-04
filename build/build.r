
#' Build `*_rows_*()` and `*_cols_*()` biplot layers.

library(stringr)
devtools::load_all()

root_args <- c(
  "mapping", "data", "stat", "geom", "position",
  "show.legend", "inherit.aes"
)

arg_c <- function(x, y, indent = 0L) {
  stopifnot(length(x) == length(y))
  y[seq(length(y) - 1L)] <- str_c(y[seq(length(y) - 1L)], ",")
  ind <- str_c(c("\n", rep(" ", indent)), collapse = "")
  str_c(x, y, sep = " = ", collapse = ind)
}

build_biplot_geom <- function(layer, .matrix) {
  .matrix <- match_factor(.matrix)
  
  # verify uniplot layer and derive biplot layer name
  layer_name <- rlang::enexpr(layer)
  if (! str_detect(layer_name, "^geom\\_"))
    stop("`", layer_name, "` is not a geometric object layer.")
  ggproto_name <- ggplot2:::camelize(layer_name, first = TRUE)
  layer_name <- str_replace(layer_name, "geom_", str_c("geom_", .matrix, "_"))
  #print(layer_name)
  
  # get uniplot formals (and insert any additional biplot formals)
  layer_formals <- formals(layer)
  layer_formals <- unlist(lapply(layer_formals, format))
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
  root_vals[[match("stat", root_args)]] <-
    str_c(.matrix, "_stat(stat)")
  root_vals[[match("geom", root_args)]] <- ggproto_name
  
  # define biplot layer internal parameters
  params <- setdiff(layer_args, c(root_args, "..."))
  
  # write function
  glue::glue(
    "{layer_name} <- function(\n",
    "  ",
    # -+- remove " = " after "..." -+-
    str_c(layer_args1, str_c(layer_vals1, ","), sep = " = ", collapse = "\n  "),
    "\n  ...,\n  ",
    if (layer2) arg_c(layer_args2, layer_vals2, 2L),
    "\n",
    ") {{\n",
    "  layer(\n",
    "    ",
    arg_c(root_args, root_vals, 4L),
    "\n",
    "    params = list(\n",
    "      ",
    str_c(params, str_c(params, ","), sep = " = ", collapse = "\n      "),
    "\n",
    "      ...\n",
    "    )\n",
    "  )\n",
    "}}\n"
  )
  
  # document function
  
}
