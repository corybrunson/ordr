#' 1. fortify
#' 2. ggbiplot (calls fortify)
#' 3. StatU & StatV (recognizes fortify output)
#' 4. GeomU* & GeomV* (call StatU & StatV)

#' 1. Have geom_u_* pass get_u to data parameter

#' @rdname ggbiplot
#' @export
stat_biplot <- function(
  mapping = NULL, data = NULL,
  geom = "biplot", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBiplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot
#' @export
StatBiplot <- ggproto(
  "StatBiplot",
  StatIdentity,
  
  compute_layer = function(data, scales, params) {
    data
  }
)

#' @rdname ggbiplot
#' @export
geom_biplot <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBiplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot
#' @export
GeomBiplot <- ggproto(
  "GeomBiplot",
  GeomPoint,
  
  required_aes = c("x", "y", ".matrix"), # need a better name than 'matrix'
  non_missing_aes = unique(c(
    GeomPoint$non_missing_aes,
    GeomSegment$non_missing_aes,
    matrix = NA
  )),
  default_aes = aes(
    #u_shape = 19, u_colour = "black", u_size = 1.5,
    #u_fill = NA, u_alpha = NA, u_stroke = .5,
    #v_colour = "darkgrey", v_size = 0.5, v_linetype = 1, v_alpha = NA,
    shape = 19, colour = "black", size = 1,
    fill = NA, alpha = NA, stroke = .5, linetype = 1
  ),
  
  draw_key = draw_key_abline, # point with line through it?
  draw_panel = function(
    data, panel_params, coord,
    arrow = grid::arrow(), lineend = "round", linejoin = "mitre",
    na.rm = FALSE
  ) {
    # ensure that data consists of two types (points and vectors)
    data$.matrix <- as.numeric(as.factor(xtfrm(data$.matrix)))
    stopifnot(all(sort(unique(data$.matrix)) == 1:2))
    # separate grobs for points and vectors
    # INVOKE 'inertia' HERE
    u_data <- data[data$.matrix == 1, -match(".matrix", names(data))]
    v_data <- data[data$.matrix == 2, -match(".matrix", names(data))]
    # DISALLOW NONLINEAR TRANSFORMATIONS?
    u_coords <- coord$transform(u_data, panel_params)
    v_coords <- coord$transform(v_data, panel_params)
    
    # construct 'u' grob
    uGrob <- grid::pointsGrob(
      u_coords$x, u_coords$y,
      pch = u_coords$shape,
      gp = grid::gpar(
        col = alpha(u_coords$colour, u_coords$alpha),
        fill = alpha(u_coords$fill, u_coords$alpha),
        fontsize = u_coords$size * .pt + u_coords$stroke * .stroke / 2,
        lwd = u_coords$stroke * .stroke / 2
      )
    )
    # construct 'v' grob (requires processing in stat layer)
    vGrob <- grid::segmentsGrob(
      #v_coords$x, v_coords$y, v_coords$xend, v_coords$yend,
      0, 0, v_coords$x, v_coords$y,
      default.units = "native",
      arrow = arrow,
      gp = grid::gpar(
        col = alpha(v_coords$colour, v_coords$alpha),
        fill = alpha(v_coords$colour, v_coords$alpha),
        lwd = v_coords$size * .pt,
        lty = v_coords$linetype,
        lineend = lineend, linejoin = linejoin
      )
    )
    
    # combine grobs
    grid::gTree(children = grid::gList(uGrob, vGrob))
  }
)
