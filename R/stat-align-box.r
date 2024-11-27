#' @title Delimit bounding boxes aligned with axes
#'
#' @description This stat layer obtains the `lower` and `upper` bounds of the
#'   projections of a referent point cloud onto an axis, and the smaller
#'   vector (`x0`, `y0`) necessary to offset the axis beyond the point cloud.
#' 

#' @template biplot-layers

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatAlignBox <- ggproto(
  "StatAlignBox", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {
    if (! is.null(params$.referent)) {
      ref <- data[data$.matrix %in% params$.referent, c("x", "y"), drop = FALSE]
      ref <- ref[chull(ref), , drop = FALSE]
      params$referent <-ref
    }
    
    params
  },
  
  setup_data = setup_cols_data,
  
  # TODO: Should this be `compute_panel()`?
  compute_group = function(
    data, scales,
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    subset = NULL, elements = "all"
  ) {
    if (is.null(referent)) return(data)
    
    # compute dimensions of alignment boxes from '.referent' attribute
    # use `x0` & `y0` for the offset and `lower` & `upper` for the extent
    # TODO: Alternatively, store summarized referent data as a data attribute.
    # ref <- attr(data, "referent") |> dplyr::transmute(x_ = x, y_ = y)
    referent <- referent |> 
      stats::setNames(c("x_", "y_")) |> 
      dplyr::mutate(
        mag_ = sqrt(x_^2 + y_^2),
        th_ = atan2(y_, x_)
      )
    # FIXME: `lower` and `upper` may suffer from a unit conversion.
    data |> 
      dplyr::mutate(angle = atan2(y, x)) |> 
      dplyr::mutate(.n = dplyr::row_number()) |> 
      tidyr::crossing(referent) |> 
      dplyr::group_by(across(all_of(c(names(data), "angle", ".n")))) |> 
      # projections of referent data onto axis and its normal vector
      dplyr::mutate(
        h = mag_ * cos(th_ - angle),
        v = mag_ * sin(th_ - angle)
      ) |> 
      dplyr::summarize(dplyr::across(
        c(h, v),
        list(min = fun.min, max = fun.max)
      )) |> 
      dplyr::ungroup() |> dplyr::select(-.n) |> 
      dplyr::mutate(
        lower = h_min, upper = h_max,
        # yintercept = 
        #   ifelse(abs(v_min) < abs(v_max), v_min, v_max) / cos(angle),
        v_off = ifelse(abs(v_min) < abs(v_max), v_min, v_max),
        x0 = v_off * cos(angle + pi/2),
        y0 = v_off * sin(angle + pi/2)
      ) |> 
      dplyr::select(-c(starts_with("h_"), starts_with("v_"))) -> 
      data
    
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
stat_align_box <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    .referent = c("rows", "cols"), referent = NULL,
    fun.min = minpp, fun.max = maxpp,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatAlignBox,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent, referent = NULL,
      fun.min = fun.min, fun.max = fun.max,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsAlignBox <- ggproto(
  "StatRowsAlignBox", StatAlignBox,
  
  setup_data = setup_rows_xy_data
)

#' @rdname biplot-stats
#' @export
stat_rows_align_box <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    ...,
    fun.min = minpp, fun.max = maxpp,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = StatRowsAlignBox,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.min = fun.min, fun.max = fun.max,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsAlignBox <- ggproto(
  "StatColsAlignBox", StatAlignBox,
  
  setup_data = setup_cols_xy_data
)

#' @rdname biplot-stats
#' @export
stat_cols_align_box <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    ...,
    fun.min = minpp, fun.max = maxpp,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = StatColsAlignBox,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.min = fun.min, fun.max = fun.max,
      na.rm = na.rm,
      ...
    )
  )
}

minpp <- function(x, p = .1) min(x) - diff(range(x)) * p
maxpp <- function(x, p = .1) max(x) + diff(range(x)) * p
