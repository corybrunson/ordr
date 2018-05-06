
as_bibble.lsvd <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_uv_lsvd <- function(x, matrix) {
  res <- x[[switch(matrix, u = "A", v = "B")]] %>%
    as_tibble() %>%
    setNames(paste0("SC", 1:ncol(.)))
  if (matrix == "v") res <- mutate(res, .mu = x$mu)
  res
}
get_u.lsvd <- function(x) get_uv_lsvd(x, "u")
get_v.lsvd <- function(x) get_uv_lsvd(x, "v")
get_coordinates.lsvd <- function(x) {
  tibble(
    .name = paste0("SC", 1:ncol(x$A))
  )
}

reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}
