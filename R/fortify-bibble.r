
#' @rdname ggbiplot
#' @export
fortify.bbl <- function(
  ordination,
  .matrix = "uv",
  include = "shared"
) {
  .matrix <- match_factor(.matrix)
  if (grepl("u", .matrix)) {
    u <- mutate(bind_cols(
      as_tibble(get_u(ordination)),
      u_attr(ordination)
    ), .matrix = "u")
  }
  if (grepl("v", .matrix)) {
    v <- mutate(bind_cols(
      as_tibble(get_v(ordination)),
      v_attr(ordination)
    ), .matrix = "v")
  }
  
  switch(
    .matrix,
    u = u,
    v = v,
    uv = switch(
      match.arg(include, c("coordinates", "shared", "all")),
      coordinates = {
        coord <- get_coord(ordination)
        as_tibble(as.data.frame(rbind(u[coord], v[coord])))
      },
      shared = {
        int <- intersect(names(u), names(v))
        as_tibble(as.data.frame(rbind(u[int], v[int])))
      },
      all = as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
    )
  )
}
