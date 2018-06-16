#' Fortify a bibble for plotting
#' 
#' These methods of \code{\link[ggplot2]{fortify}} and \code{\link[broom]{tidy}}
#' convert a \code{\link{bibble}} to a \code{\link{tibble}}.
#' 

#' @name bibble-fortification
#' @param model,x A \link{bibble}, i.e. an ordination object of class 
#'   \code{"bbl"}.
#' @param data Ignored.
#' @param ... Additional arguments received from \code{fortify} or \code{tidy};
#'   ignored.
#' @template matrix-param
#' @param include Character matched to \code{"coordinates"}, \code{"shared"}, or
#'   \code{"all"}; whether the fortified data frame should include only the 
#'   ordination coordinates, also the annotations shared by subjects and 
#'   variables, or all annotations (with \code{NA}s where not defined).
#' @example inst/examples/ex-bibble-lm.r

#' @rdname bibble-fortification
#' @export
fortify.bbl <- function(
  model, data, ...,
  .matrix = "uv",
  include = "all"
) {
  .matrix <- match_factor(.matrix)
  include <- match.arg(include, c("coordinates", "shared", "all"))
  
  if (grepl("u", .matrix)) {
    u <- as_tibble(get_u(model))
    if (include != "coordinates") {
      u <- dplyr::bind_cols(
        u,
        u_annot(model)
      )
      u$.matrix <- "u"
    }
  }
  if (grepl("v", .matrix)) {
    v <- as_tibble(get_v(model))
    if (include != "coordinates") {
      v <- dplyr::bind_cols(
        v,
        v_annot(model)
      )
      v$.matrix <- "v"
    }
  }
  
  switch(
    .matrix,
    u = u,
    v = v,
    uv = switch(
      include,
      #coordinates = {
      #  coord <- get_coord(model)
      #  as_tibble(as.data.frame(rbind(u[coord], v[coord])))
      #},
      coordinates = as_tibble(as.data.frame(rbind(u, v))),
      shared = {
        int <- intersect(names(u), names(v))
        as_tibble(as.data.frame(rbind(u[int], v[int])))
      },
      all = as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
    )
  )
}

#' @rdname bibble-fortification
#' @export
fortify_u <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "u", include = include)
}

#' @rdname bibble-fortification
#' @export
fortify_v <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "v", include = include)
}

#' @rdname bibble-fortification
#' @importFrom broom tidy
#' @export
tidy.bbl <- function(x, ..., .matrix = "uv", include = "all") {
  fortify.bbl(model = x, data = NULL, .matrix = .matrix, include = include)
}
