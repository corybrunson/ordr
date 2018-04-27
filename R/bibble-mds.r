cmdscale <- function(d, k = 2, add = FALSE) {
  res <- stats::cmdscale(d, k = k, eig = TRUE, add = add, x.ret = TRUE)
  class(res) <- "cmds"
  res
}

as_bibble.cmds <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_uv_cmds <- function(x, matrix) {
  name_fun <- switch(matrix, u = rownames, v = colnames)
  x$points %>%
    as_tibble() %>%
    {if (is.null(name_fun(x$x))) . else mutate(., name = name_fun(x$x))} %>%
    rename_at(vars(matches("^V[1-9]+$")), funs(gsub("^V", "PCo", .))) %>%
    select(starts_with("PCo"), everything())
}
get_u.cmds <- function(x) get_uv_cmds(x, "u")
get_v.cmds <- function(x) get_uv_cmds(x, "v")
get_coordinates.cmds <- function(x) {
  tibble(
    .name = paste0("PCo", 1:ncol(x$points)),
    eigenvalue = x$eig[1:ncol(x$points)]
  )
}
