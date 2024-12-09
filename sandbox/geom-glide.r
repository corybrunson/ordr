GeomGlide <- ggproto(
  "GeomGlide", GeomGlide,
  
  required_aes = c("x", "y"),
  
  draw_panel = function(
    data, panel_params, coord,
    na.rm = FALSE
  ) {
    
    data
    
  }
)

geom_rule <- function(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlide,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
