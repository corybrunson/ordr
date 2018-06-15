
# This is a method of `ggplot2::fortify()` for bibbles.
fortify.bbl <- function(
  ordination,
  .matrix = "uv",
  include = "all"
) {
  .matrix <- match_factor(.matrix)
  if (grepl("u", .matrix)) {
    u <- bind_cols(
      as_tibble(get_u(ordination)),
      u_annot(ordination)
    )
    u$.matrix <- "u"
  }
  if (grepl("v", .matrix)) {
    v <- bind_cols(
      as_tibble(get_v(ordination)),
      v_annot(ordination)
    )
    v$.matrix <- "v"
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
