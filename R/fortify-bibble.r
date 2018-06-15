#' Fortify a bibble for plotting
#' 
#' @name bibble-fortification
#' @param model A \link{bibble}, i.e. an ordination object of class 
#'   \code{"bbl"}.
#' @param data Ignored.
#' @param ... Additional arguments received from \code{fortify}; ignored.
#' @template matrix-param
#' @param include Character matched to \code{"coordinates"}, \code{"shared"}, or
#'   \code{"all"}; whether the fortified data frame should include only the
#'   ordination coordinates, also the annotations shared by subjects and
#'   variables, or all annotations (with \code{NA}s where not defined).
#' @example inst/examples/ex-bibble-lm.r
#' @export
fortify.bbl <- function(
  model, data, ...,
  .matrix = "uv",
  include = "all"
) {
  .matrix <- match_factor(.matrix)
  if (grepl("u", .matrix)) {
    u <- dplyr::bind_cols(
      as_tibble(get_u(model)),
      u_annot(model)
    )
    u$.matrix <- "u"
  }
  if (grepl("v", .matrix)) {
    v <- dplyr::bind_cols(
      as_tibble(get_v(model)),
      v_annot(model)
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
        coord <- get_coord(model)
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
