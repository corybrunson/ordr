#' @title Tidiers for 'tbl_ord' objects
#'
#' @description These functions return [tibbles][tibble::tibble] that summarize
#'   an object of class '[tbl_ord]'. `tidy()` output contains one row per
#'   artificial coordinate and `glance()` output contains one row for the whole
#'   ordination.
#'   

#' @details
#' 
#' Three generics popularized by the **ggplot2** and **broom** packages make use
#' of the [augmentation] methods:
#' 

#' * The [generics::tidy()] method

#'   summarizes information about model components, which here are the
#'   artificial coordinates created by ordinations. The output can be passed to
#'   [ggplot2::ggplot()] to generate scree plots.

#' * The [generics::glance()] method

#'   reports information about the entire model, here always treated as one of a
#'   broader class of ordination models.

#' * The [ggplot2::fortify()] method

#'   augments and collapses row and/or column data, depending on `.matrix` and
#'   `.supplement`, into a single tibble, in preparation for
#'   [ggplot2::ggplot()]. Its output resembles that of [generics::augment()],
#'   though rows in the output may correspond to rows, columns, or both of the
#'   original data. If `.matrix` is passed `"rows"`, `"cols"`, or `"dims"` (for
#'   both), then `fortify()` returns a tibble whose fields are obtained, in
#'   order, via `get_*()`, `augmentation_*()`, and `annotation_*()`.

#'
#' The tibble is assigned a `"coordinates"` attribute whose value is obtained
#' via [get_coord()]. This facilitates some downstream functionality that relies
#' on more than those coordinates used as position aesthetics in a biplot, in
#' particular [stat_spantree()].

#' @name tidiers
#' @include ord-accessors.r
#' @param x,model An object of class '[tbl_ord]'.
#' @param data Passed to generic methods; currently ignored.
#' @param ... Additional arguments allowed by generics; currently ignored.
#' @template param-matrix
#' @param elements Character; which elements of each factor for which to render
#'   graphical elements. One of `"all"` (the default), `"active"`, or
#'   `"supplementary"`, with partial matching.
#' @seealso [augmentation] methods that must interface with tidiers.
#' @example inst/examples/ex-ord-tidiers.r
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
  all.var <- recover_inertia(as_tbl_ord(x))
  tot.var <- sum(all.var)
  var.na <- identical(all.var, NA_real_)
  tibble::tibble(
    # number of artificial coordinates
    rank = length(recover_inertia(x)),
    # numbers of rows and of columns of original data
    # -+- these should indicate dimensions of decomposed matrix -+-
    n.row = nrow(x),
    n.col = ncol(x),
    # -+- clarify whether this is original inertia or decomposed inertia -+-
    inertia = tot.var,
    # proportions of variance/inertia in first and second artificial dimensions
    prop.var.1 = if (var.na) NA else all.var[[1L]] / tot.var,
    prop.var.2 = if (var.na) NA else all.var[[2L]] / tot.var,
    # original ordination object (first) class
    class = setdiff(class(x), "tbl_ord")[[1L]]
  )
}

#' @rdname tidiers
#' @export
fortify.tbl_ord <- function(
  model, data, ...,
  .matrix = "dims", elements = "all"
) {
  .matrix <- match_factor(.matrix)
  elements <- match.arg(elements, c("all", "active", "supplementary"))
  
  if (.matrix == "dims" || .matrix == "rows") {
    u <- as_tibble(get_rows(model))
    u <- dplyr::bind_cols(u, annotation_factor(model, "rows"))
    if (elements != "all" && ".supplement" %in% names(u)) {
      u <- switch(
        elements,
        active = u[! u$.supplement, , drop = FALSE],
        supplementary = u[u$.supplement, , drop = FALSE]
      )
    }
    u$.matrix <- "rows"
    if (! ".supplement" %in% names(u)) u$.supplement <- NA
  }
  if (.matrix == "dims" || .matrix == "cols") {
    v <- as_tibble(get_cols(model))
    v <- dplyr::bind_cols(v, annotation_factor(model, "cols"))
    if (elements != "all" && ".supplement" %in% names(v)) {
      v <- switch(
        elements,
        active = v[! v$.supplement, , drop = FALSE],
        supplementary = v[v$.supplement, , drop = FALSE]
      )
    }
    v$.matrix <- "cols"
    if (! ".supplement" %in% names(v)) v$.supplement <- NA
  }
  
  tbl <- switch(
    .matrix,
    rows = u,
    cols = v,
    dims = as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
  )
  
  attr(tbl, "coordinates") <- get_coord(model)
  tbl
}
