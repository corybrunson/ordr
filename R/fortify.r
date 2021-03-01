#' @title Fortify a tbl_ord for plotting
#'
#' @description This [ggplot2::fortify()] method converts a 'tbl_ord' object to
#'   a ['tbl_df'][tibble::tbl_df] object.
#'   

#' @details
#'
#' The `fortify()` method for 'tbl_ord' objects produces the same
#' output, a [tibble][tibble::tibble] obtained by binding columns obtained via
#' [`get_*()`][accessors], [`annotation_*()`][annotation], and
#' [`augmentation_*`][augmentation], respectively.
#'
#' In the special case `.matrix = "coordinates"`, the tibble consists instead of
#' the artificial coordinates and associated metadata, often including the
#' proportion of variance explained or captured by each. This can be used to
#' produce scree plots, for example.
#'
#' If any augmented variables are included (i.e. unless `include =
#' "coordinates"`), then the tibble is assigned a `"coordinates"` attribute
#' whose value is obtained via [get_coord()]. (That this attribute will not be
#' printed with the tibble.) This facilitates some downstream functionality that
#' relies on more than those coordinates used as position aesthetics in a
#' biplot, in particular [stat_spantree()].

#' @name fortify
#' @param model A '[tbl_ord]' object.
#' @param data Ignored.
#' @param ... Additional arguments received from `fortify()`; ignored.
#' @template param-matrix
#' @param .supplement Logical; whether to include
#'   [supplementary][supplementation] points.
#' @param include Character matched to `"coordinates"`, `"shared"`, or `"all"`;
#'   whether the fortified data frame should include only the ordination
#'   coordinates or also augmented case and variable data, and, if the latter,
#'   whether only shared fields or all from both.

#' @rdname fortify
#' @export
fortify.tbl_ord <- function(
  model, data, ...,
  .matrix = "dims", .supplement = TRUE,
  include = "all"
) {
  # check first if coordinate / inertia diagonal is desired
  .matrix <- match.arg(.matrix, c(names(tbl_ord_factors), "coordinates"))
  if (.matrix == "coordinates") {
    # (ignore `include`)
    return(fortify_coord(model))
  }
  # otherwise resume fortification of matrix factors
  .matrix <- unname(tbl_ord_factors[.matrix])
  include <- match.arg(include, c("coordinates", "shared", "all"))
  
  if (.matrix == "dims" || .matrix == "rows") {
    u <- as_tibble(get_rows(model))
    if (include != "coordinates") {
      u <- dplyr::bind_cols(
        u,
        augment_annotation(model, "rows")
      )
    }
    if (! .supplement && ".supplement" %in% names(u)) {
      u <- subset(u, ! .supplement)
      u$.supplement <- NULL
    }
    if (include != "coordinates") {
      u$.matrix <- "rows"
    }
  }
  if (.matrix == "dims" || .matrix == "cols") {
    v <- as_tibble(get_cols(model))
    if (include != "coordinates") {
      v <- dplyr::bind_cols(
        v,
        augment_annotation(model, "cols")
      )
    }
    if (! .supplement && ".supplement" %in% names(v)) {
      v <- subset(v, ! .supplement)
      v$.supplement <- NULL
    }
    if (include != "coordinates") {
      v$.matrix <- "cols"
    }
  }
  
  tbl <- switch(
    .matrix,
    rows = u,
    cols = v,
    dims = switch(
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

#' @rdname fortify
#' @export
fortify_coord <- function(model) {
  tbl <- tidy(model)
  tbl$.prop_var <- tbl$.inertia / sum(tbl$.inertia)
  tbl
}

#' @rdname fortify
#' @export
fortify_rows <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "rows", include = include)
}

#' @rdname fortify
#' @export
fortify_cols <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "cols", include = include)
}
