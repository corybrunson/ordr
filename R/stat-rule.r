#' @title Construct limited rulers offset from the origin
#' 
#' @description Determine axis limits and offset vectors from reference data.
#' 

#' @template biplot-layers

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRule <- ggproto(
  "StatRule", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {
    # TODO: Find a better way to handle this; are two parameters necessary?
    if (! is.null(params$.referent)) {
      ref <- data[data$.matrix %in% params$.referent, c("x", "y"), drop = FALSE]
      ref <- ref[chull(ref), , drop = FALSE]
      params$referent <- ref
    }
    
    # if summary functions are `NULL` then use zero constant functions
    params$fun.min <- params$fun.min %||% const0
    params$fun.max <- params$fun.max %||% const0
    params$fun.offset <- params$fun.offset %||% const0
    
    params
  },
  
  setup_data = setup_cols_data,
  
  # TODO: Should this be `compute_panel()`?
  compute_group = function(
    data, scales,
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
    subset = NULL, elements = "all"
  ) {
    
    if (is.null(referent)) {
      data$x0 <- data$y0 <- 0
      data$xend <- data$yend <- NA_real_
      return(data)
    }
    
    # compute dimensions of aligned boxes from '.referent' attribute
    # use `x0` & `y0` for offset and `x`, `y`, `xend`, & `yend` for extent
    referent <- referent |> 
      stats::setNames(c("x_", "y_")) |> 
      dplyr::mutate(
        radius_ = sqrt(x_^2 + y_^2),
        radians_ = atan2(y_, x_)
      )
    # unambiguous name for original data row number
    row_name <- ".n"
    while (row_name %in% names(data)) row_name <- paste0(".", row_name)
    # compute projections of all referent points to each axis
    data |> 
      dplyr::mutate(
        radius = sqrt(x^2 + y^2),
        radians = atan2(y, x)
      ) |> 
      dplyr::mutate(!! row_name := dplyr::row_number()) |> 
      tidyr::crossing(referent) |> 
      dplyr::group_by(dplyr::across(
        tidyselect::all_of(c(names(data), "radius", "radians", row_name))
      )) |> 
      # projections of referent points onto axis and its normal vector
      dplyr::mutate(
        h = radius_ * cos(radians_ - radians),
        v = radius_ * sin(radians_ - radians)
      ) |> 
      # compute offsets and endpoints
      dplyr::summarize(
        h_min = fun.min(h),
        h_max = fun.max(h),
        v_off = fun.offset(v)
      ) |> 
      dplyr::mutate(
        xend = h_min * cos(radians),        yend = h_min * sin(radians),
        x    = h_max * cos(radians),        y    = h_max * sin(radians),
        x0   = v_off * cos(radians + pi/2), y0   = v_off * sin(radians + pi/2)
      ) |>
      # drop unused variables
      dplyr::select(-c(ends_with("_"))) |>
      dplyr::select(-c(starts_with("h_"), starts_with("v_"))) ->
      data
    data$angle <- (180 / pi) * data$radians
    data$radians <- NULL
    
    # TODO: Check that `xend,yend` and `x,y` are collinear with the origin.

    # TODO: Document computed variables:
    # xend,yend,x,y,x0,y0,radius,angle
    data
  }
)

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param referent The point cloud to rule; a data frame with `x` and `y`
#'   columns or a subset of `c("rows", "cols")` to indicate row and/or column
#'   elements from an ordination model.
#' @template return-layer
#' @family stat layers
#' @examples
#' @export
stat_rule <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent, referent = NULL,
      fun.min = fun.min, fun.max = fun.max,
      fun.offset = fun.offset,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsRule <- ggproto(
  "StatRowsRule", StatRule,
  
  setup_data = setup_rows_xy_data
)

#' @rdname biplot-stats
#' @export
stat_rows_rule <- function(
    mapping = NULL,
    data = NULL,
    geom = "axis",
    position = "identity",
    ...,
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = StatRowsRule,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent, referent = NULL,
      fun.min = fun.min, fun.max = fun.max,
      fun.offset = fun.offset,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsRule <- ggproto(
  "StatColsRule", StatRule,
  
  setup_data = setup_cols_xy_data
)

#' @rdname biplot-stats
#' @export
stat_cols_rule <- function(
    mapping = NULL,
    data = NULL,
    geom = "axis",
    position = "identity",
    ...,
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    fun.offset = minabspp,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = StatColsRule,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent, referent = NULL,
      fun.min = fun.min, fun.max = fun.max,
      fun.offset = fun.offset,
      na.rm = na.rm,
      ...
    )
  )
}

# convenience functions for rule limits and axis offset
const0 <- function(x) 0
minpp <- function(x, p = .1) min(x) - diff(range(x)) * p
maxpp <- function(x, p = .1) max(x) + diff(range(x)) * p
minabspp <- function(x, p = .1) {
  minmaxpp <- c(minpp(x, p), maxpp(x, p))
  minmaxpp[which.min(abs(minmaxpp))]
}
