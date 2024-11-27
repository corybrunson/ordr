#' @title Delimit bounding boxes aligned with axes
#'
#' @description This stat layer obtains the `lower` and `upper` bounds of the
#'   projections of a referent point cloud onto an axis, and the smaller
#'   `yintercept` necessary to offset the axis beyond the point cloud.
#' 

#' @template biplot-layers

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @examples
#' @export
stat_align_box <- function(
    mapping = NULL, data = NULL, geom = "axis", position = "identity",
    fun.min = min, fun.max = max,
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
    fun.min = min, fun.max = max,
    subset = NULL, elements = "all"
  ) {
    # compute dimensions of alignment boxes from '.referent' attribute
    # use `yintercept` for offset and `lower` & `upper` for extent
    # TODO: Alternatively, store summarized referent data as a data attribute.
    # ref <- attr(data, "referent") |> dplyr::transmute(x_ = x, y_ = y)
    ref <- params$referent |> 
      stats::setNames(c("x_", "y_")) |> 
      dplyr::mutate(
        mag_ = sqrt(x_^2 + y_^2),
        th_ = atan2(y_, x_) %% (2*pi)
      )
    data |> 
      dplyr::mutate(h_angle = atan2(y, x)) |> 
      dplyr::mutate(.n = dplyr::row_number()) |> 
      tidyr::crossing(ref) |> 
      dplyr::group_by(across(all_of(c(names(data), ".n")))) |> 
      dplyr::mutate(
        h = mag_ * cos(th_ - h_angle),
        v = mag_ * cos(th_ - h_angle - pi/2)
      ) |> 
      dplyr::summarize(dplyr::across(
        c(h, v),
        list(min = params$fun.min, max = params$fun.max)
      )) |> 
      dplyr::ungroup() |> dplyr::select(-.n) |> 
      dplyr::mutate(
        lower = h_min, upper = h_max,
        yintercept = 
          ifelse(abs(v_min) > abs(v_max), v_min, v_max) / cos(h_angle)
      ) |> 
      dplyr::select(-c(starts_with("h_"), starts_with("v_"))) -> 
      data
    
    data
  }
)
