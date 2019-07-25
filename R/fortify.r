#' @title Fortify a `tbl_ord` for plotting
#'
#' @description These methods of [ggplot2::fortify()] and [generics::tidy()]
#'   convert a `[tbl_ord]` object to a [tbl_df][tibble::tbl_df] object.
#'   

#' @details
#' 
#' The `fortify()` and `tidy()` methods for `tbl_ord` objects produce the same
#' output, a [tibble][tibble::tibble] obtained by binding columns obtained via
#' [`get_*()`][accessors], [`annotation_*()`][annotation], and
#' [`augmentation_*`][augmentation], respectively.
#'
#' If any augmented variables are included
#' (i.e. unless `include = "coordinates"`),
#' then the tibble is assigned a `"coordinates"` attribute
#' whose value is obtained via [get_coord()].
#' (Note that this attribute will not be printed with the tibble.)

#' @name fortify
#' @param model,x A `[tbl_ord]` object.
#' @param data Ignored.
#' @param ... Additional arguments received from `fortify()` or `tidy()`;
#'   ignored.
#' @template param-matrix
#' @param include Character matched to `"coordinates"`, `"shared"`, or `"all"`;
#'   whether the fortified data frame should include only the ordination
#'   coordinates or also augmented case and variable data, and, if the latter,
#'   whether only shared fields or all from both.
#' @example inst/examples/ex-fortify.r
#' @example inst/examples/diabetes-lda.r

#' @rdname fortify
#' @export
fortify.tbl_ord <- function(
  model, data, ...,
  .matrix = "uv",
  include = "all", supplement = TRUE
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
  
  if (grepl("u", .matrix)) {
    u <- as_tibble(get_u(model))
    if (include != "coordinates") {
      u <- dplyr::bind_cols(
        u,
        augment_annotation(model, "u")
      )
      u$.matrix <- "u"
    }
    if (supplement && ! is.null(attr(model, "u_supplement"))) {
      u <- dplyr::bind_rows(u, attr(model, "u_supplement"))
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
    if (supplement && ! is.null(attr(model, "v_supplement"))) {
      v <- dplyr::bind_rows(v, attr(model, "v_supplement"))
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

#' @rdname fortify
#' @export
fortify_coord <- function(model) {
  tbl <- bind_cols(
    augmentation_coord(model),
    .inertia = recover_inertia(model)
  )
  tbl$.prop_var <- tbl$.inertia / sum(tbl$.inertia)
  tbl
}

#' @rdname fortify
#' @export
fortify_u <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "u", include = include)
}

#' @rdname fortify
#' @export
fortify_v <- function(model, include = "all") {
  include <- match.arg(include, c("coordinates", "all"))
  fortify(model = model, data = NULL, .matrix = "v", include = include)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname fortify
#' @export
tidy.tbl_ord <- function(x, ..., .matrix = "uv", include = "all") {
  fortify.tbl_ord(model = x, data = NULL, .matrix = .matrix, include = include)
}
