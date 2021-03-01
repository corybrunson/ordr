#' @title Tidiers for 'tbl_ord's
#'
#' @description These functions return [tibbles][tibble::tibble] that summarize
#'   a '[tbl_ord]' object. `tidy()` output contains one row per artificial
#'   coordinate and `glance()` output contains one row for the whole ordination.
#'   

#' @details
#' 
#' Three generics popularized by the **ggplot2** and **broom** packages make use
#' of the [augmentation] methods:
#' 

#' * The [generics::tidy()] method

#'   summarizes information about model components, which here are the
#'   artificial coordinates created by ordinations.

#' * The [generics::glance()] method

#'   reports information about the entire model, here always treated as one of a
#'   broader class of ordination models.

#' * The [ggplot2::fortify()] method

#'   augments and collapses row and/or column data, depending on `.matrix`, into
#'   a single tibble, in preparation for [ggplot2::ggplot()]. Its output
#'   resembles that of [generics::augment()], though rows in the output may
#'   correspond to rows, columns, or both of the original data.

#' 
#' If `.matrix` is passed `"rows"`, `"cols"`, or `"dims"` (for both), then
#' `fortify()` returns a tibble whose fields are obtained, in order, via
#' `get_*()`, `augmentation_*()`, and `annotation_*()`. In the special case
#' `.matrix = "coordinates"`, the augmented artificial coordinates are returned.
#' This can be used to produce scree plots, for example.
#'
#' If augmentation or any annotation is included, then the tibble is assigned a
#' `"coordinates"` attribute whose value is obtained via [get_coord()]. This
#' facilitates some downstream functionality that relies on more than those
#' coordinates used as position aesthetics in a biplot, in particular
#' [stat_spantree()].

#' @name tidiers
#' @include ord-accessors.r
#' @inheritParams accessors
#' @param x,model A '[tbl_ord]' object.
#' @param data Passed to generic methods; currently ignored.
#' @param ... Additional arguments allowed by generics; currently ignored.
#' @template param-matrix
#' @param .supplement Logical; whether to include
#'   [supplementary][supplementation] points.
#' @param coord.only Logical; whether to exclude augmentation and annotation.
#' @seealso [augmentation]
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname tidiers
#' @export
tidy.tbl_ord <- function(x, ...) {
  res <- augmentation_coord(x)
  res$.inertia <- recover_inertia(x)
  res$.prop_var <- res$.inertia / sum(res$.inertia)
  res
}

#' @importFrom generics glance
#' @export
generics::glance

#' @rdname tidiers
#' @export
glance.tbl_ord <- function(x, ...) {
  tibble::tibble(
    # number of artificial coordinates
    rank = dim(x),
    # numbers of rows and of columns of original data
    n.row = nrow(get_rows(x)),
    n.col = ncol(get_rows(x)),
    # -+- clarify whether this is original inertia or decomposed inertia -+-
    inertia = sum(recover_inertia(as_tbl_ord(x))),
    # original ordination object class
    class = setdiff(class(x), "tbl_ord")[[1L]]
  )
}

#' @rdname tidiers
#' @export
fortify.tbl_ord <- function(
  model, data, ...,
  .matrix = "dims", .supplement = TRUE,
  coord.only = FALSE
) {
  # check first if coordinate / inertia diagonal is desired
  .matrix <- match.arg(.matrix, c(names(tbl_ord_factors), "coordinates"))
  if (.matrix == "coordinates") {
    # (ignore `coord.only`)
    return(tidy(model))
  }
  # otherwise resume fortification of matrix factors
  .matrix <- unname(tbl_ord_factors[.matrix])
  
  if (.matrix == "dims" || .matrix == "rows") {
    u <- as_tibble(get_rows(model))
    if (! coord.only) {
      u <- dplyr::bind_cols(
        u,
        augment_annotation(model, "rows")
      )
    }
    if (! .supplement && ".supplement" %in% names(u)) {
      u <- subset(u, ! .supplement)
      u$.supplement <- NULL
    }
    if (! coord.only) {
      u$.matrix <- "rows"
    }
  }
  if (.matrix == "dims" || .matrix == "cols") {
    v <- as_tibble(get_cols(model))
    if (! coord.only) {
      v <- dplyr::bind_cols(
        v,
        augment_annotation(model, "cols")
      )
    }
    if (! .supplement && ".supplement" %in% names(v)) {
      v <- subset(v, ! .supplement)
      v$.supplement <- NULL
    }
    if (! coord.only) {
      v$.matrix <- "cols"
    }
  }
  
  tbl <- switch(
    .matrix,
    rows = u,
    cols = v,
    dims = if (coord.only) {
      as_tibble(as.data.frame(rbind(u, v)))
    } else {
      as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
    }
  )
  
  if (! coord.only) {
    attr(tbl, "coordinates") <- get_coord(model)
  }
  tbl
}
