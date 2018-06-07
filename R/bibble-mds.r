
cmdscale <- function(d, k = 2, add = FALSE) {
  res <- stats::cmdscale(d, k = k, eig = TRUE, add = add, x.ret = TRUE)
  class(res) <- "cmds"
  res
}

as_bibble.cmds <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_uv_cmds <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x$points
  dimnames(res) <- list(
    dimnames(x$x)[[switch(.matrix, u = 1, v = 2)]],
    get_coord(x)
  )
  res
}
get_u.cmds <- function(x) get_uv_cmds(x, "u")
get_v.cmds <- function(x) get_uv_cmds(x, "v")

get_coord.cmds <- function(x) paste0("PCo", 1:ncol(x$points))

u_attr.cmds <- function(x) {
  tibble(
    .name = dimnames(x$x)[[1]]
  )
}

v_attr.cmds <- function(x) {
  tibble(
    .name = dimnames(x$x)[[2]]
  )
}

coord_attr.cmds <- function(x) {
  tibble(
    .name = get_coord(x),
    .eig = x$eig[1:ncol(x$points)]
  )
}
