
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

u_annot.cmds <- function(x) {
  .name <- rownames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

v_annot.cmds <- function(x) {
  .name <- colnames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

coord_annot.cmds <- function(x) {
  tibble(
    .name = get_coord(x),
    .eig = x$eig[1:ncol(x$points)]
  )
}
