#' Fortify a \code{tbl_ord} for plotting
#'
#' These methods of \code{\link[ggplot2]{fortify}} and \code{\link[broom]{tidy}}
#' convert a \code{\link{tbl_ord}} object to a
#' \code{\link[tibble:tibble]{tbl_df}} object.
#' 

#' The \code{fortify} and \code{tidy} methods for \code{tbl_ord} objects produce
#' the same output, a \link[tibble]{tibble} obtained by binding columns obtained
#' via \code{\link[=accessors]{get_*}}, \code{\link[=annotation]{annotation_*}},
#' and \code{\link[=augmentation]{augmentation_*}}, respectively.
#'
#' If any augmented variables are included (i.e. unless \code{include =
#' "coordinates"}), then the tibble is assigned a \code{"coordinates"} attribute
#' whose value is obtained via \code{\link{get_coord}}. (Note that this
#' attribute will not be printed with the tibble.)

#' @name fortification
#' @param model,x A \code{\link{tbl_ord}}.
#' @param data Ignored.
#' @param ... Additional arguments received from \code{fortify} or \code{tidy}; 
#'   ignored.
#' @template param-matrix
#' @param include Character matched to \code{"coordinates"}, \code{"shared"}, or
#'   \code{"all"}; whether the fortified data frame should include only the 
#'   ordination coordinates or also augmented case and variable data, and, if
#'   the latter, whether only shared fields or all from both.

#' @rdname fortification
#' @export
fortify.tbl_ord <- function(
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
        augment_annotation(model, "u")
      )
      u$.matrix <- "u"
    }
  }
  if (grepl("v", .matrix)) {
    v <- as_tibble(get_v(model))
    if (include != "coordinates") {
      v <- dplyr::bind_cols(
        v,
        augment_annotation(model, "v")
      )
      v$.matrix <- "v"
    }
  }
  
  tbl <- switch(
    .matrix,
    u = u,
    v = v,
    uv = switch(
      include,
      coordinates = as_tibble(as.data.frame(rbind(u, v))),
      shared = {
        int <- intersect(names(u), names(v))
        as_tibble(as.data.frame(rbind(u[int], v[int])))
      },
      all = as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
    )
  )
  
  if (include != "coordinates") {
    attr(tbl, "coordinates") <- get_coord(model)
  }
  tbl
}

#' @rdname fortification
#' @export
fortify_u <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "u", include = include)
}

#' @rdname fortification
#' @export
fortify_v <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "v", include = include)
}

#' @importFrom broom tidy
#' @export
broom::tidy

#' @rdname fortification
#' @export
tidy.tbl_ord <- function(x, ..., .matrix = "uv", include = "all") {
  fortify.tbl_ord(model = x, data = NULL, .matrix = .matrix, include = include)
}
