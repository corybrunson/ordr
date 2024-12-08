# `stat_ord_angle_box(fun.data = , fun.min = , fun.max = )`
# compute offsets based on both matrix factors

iris_pca |> 
  fortify() |> 
  rename(x = PC1, y = PC2, color = Species) |> select(-starts_with("PC")) |> 
  print() -> data
params <- list(
  angle = pi/3,
  # fun.data = \(d) dplyr::summarize(d, list(min, max))
  fun.min = min,
  fun.max = max,
  # fun.min = \(x) quantile(x, .1),
  # fun.max = \(x) quantile(x, .9),
  .referent = "rows",
  elements = "active"
)

# `StatColsRule`:
# * separate row and column data
# * within column data:
#   compute angle box dimensions for row data
#   automatically select offsets
# `GeomColsRule`:
# * additionally require offsets
# * offset axes & annotations after computing them

#' @export
stat_cols_rule <- function(
    mapping = NULL, data = NULL, geom = "cols_rule", position = "identity",
    show.legend = NA, 
    inherit.aes = TRUE,
    .referent = "rows",
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsRule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .referent = .referent,
      na.rm = FALSE,
      ...
    )
  )
}

#' @export
geom_cols_rule <- function(
    mapping = NULL, data = NULL, stat = "cols_rule", position = "identity",
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColsRule, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsRule <- ggproto(
  "StatColsRule", Stat,
  
  required_aes = c("x", "y"),
  
  setup_data = setup_cols_data,
  
  compute_group = function(
    data, scales,
    subset = NULL, elements = "all"
  ) {
    # compute dimensions of angle boxes from '.referent' attribute
    ref <- attr(data, "referent") |> dplyr::transmute(xx = x, yy = y)
    data <- within(data, {
      hslope <- y / x
      hsign <- sign(x)
      vslope <- - x / y
      # TODO: Correct these calculations.
      angle <- atan2(y, x)# %% (2*pi)
      vsign <- (-1L) ^ ((angle - pi/2) %% (2*pi) >= pi)
    })
    data |> 
      mutate(.n = dplyr::row_number()) |> 
      tidyr::crossing(ref) |> 
      dplyr::group_by(across(all_of(c(names(data), ".n")))) |> 
      dplyr::mutate(
        h = hsign * (xx + hslope * yy) / sqrt(1 + hslope^2),
        v = vsign * (xx + vslope * yy) / sqrt(1 + vslope^2)
      ) |> 
      dplyr::summarize(dplyr::across(
        c(h, v),
        list(min = params$fun.min, max = params$fun.max),
        .names = "{.col}{.fn}"
      )) |> 
      dplyr::ungroup() |> 
      dplyr::select(-.n) -> 
      data
    
    data
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomColsRule <- ggproto(
  "GeomColsRule", GeomAxis,
  
  required_aes
)



#' @rdname stat_angle_box
#' @export
stat_angle_box <- function(
    mapping = NULL, data = NULL, geom = "blank", position = "identity",
    angle = NULL,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatAngleBox,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      angle = angle,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatAngleBox <- ggproto(
  "StatAngleBox", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {
    if (! is.null(params$angle)) params$angle <- params$angle %% (2*pi)
    
    params
  },
  
  compute_group = function(
    data, scales,
    angle = NULL,
  ) {
    hslope <- tan(params$angle)
    hsign <- (-1L) ^ (params$angle >= pi)
    vslope <- - 1 / hslope
    vsign <- (-1L) ^ ((params$angle - pi/2) %% (2*pi) >= pi)
    data <- within(data, {
      h <- hsign * (x + hslope * y) / sqrt(1 + hslope^2)
      v <- vsign * (x + vslope * y) / sqrt(1 + vslope^2)
    })
    
    dplyr::summarize(
      data,
      dplyr::across(
        c(h, v),
        list(min = params$fun.min, max = params$fun.max),
        .names = "{.col}{.fn}"
      )
    )
  }
)



#' @rdname stat_rows
#' @export
stat_ord <- function(
    mapping = NULL, data = data,
    geom = "blank", position = "identity",
    subset = NULL, elements = "all",
    ...,
    show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "ord",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset, elements = elements,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatOrd <- ggproto(
  "StatOrd", StatIdentity,
  
  setup_data = setup_elts_data,
  
  compute_group = function(data, scales,
                           subset = NULL, elements = "all") {
    data
  }
)

#' @rdname stat_rows
#' @export
stat_ord_angle_box <- function(
    mapping = NULL, data = data,
    geom = "blank", position = "identity",
    subset = NULL, elements = "all",
    ...,
    show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "ord_angle_box",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset, elements = elements,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatOrdAngleBox <- ggproto(
  "StatOrdAngleBox", StatIdentity,
  
  setup_data = setup_elts_data,
  
  compute_group = function(
    data, scales,
    subset = NULL, elements = "all"
  ) {
    ord_cols <- get_ord_aes(data)
    
    
  }
)

