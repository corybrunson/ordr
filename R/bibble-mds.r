cmdscale <- function(d, k = 2, add = FALSE) {
  res <- stats::cmdscale(d, k = k, eig = TRUE, add = add, x.ret = TRUE)
  class(res) <- "cmds"
  res
}

as_bibble.cmds <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_uv_cmds <- function(x, dimension) {
  name_fun <- switch(dimension, `1` = rownames, `2` = colnames)
  x$points %>%
    as_tibble() %>%
    mutate(name = name_fun(x$x)) %>%
    rename_at(vars(matches("^V[1-9]+$")), funs(gsub("^V", "PCo", .))) %>%
    select(starts_with("PCo"), everything())
}
get_u.cmds <- function(x) get_uv_cmds(x, 1)
get_v.cmds <- function(x) get_uv_cmds(x, 2)
get_coordinates.cmds <- function(x) {
  tibble(
    .name = paste0("PCo", 1:ncol(x$points)),
    eigenvalue = x$eig[1:ncol(x$points)]
  )
}
