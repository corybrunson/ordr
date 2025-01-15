#' @title Bagplots
#'
#' @description Render bagplots from tagged data comprising medians, hulls,
#'   contours, and outlier specifications.

#' @details `geom_bagplot()` is designed to pair with [stat_bagplot()],
#'   analogously to the pairing of [ggplot2::geom_boxplot()] with
#'   [ggplot2::stat_boxplot()].
#'
#'   Because the optional components are more expensive to compute in this
#'   setting, they are controlled by parameters passed to the stat. The standard
#'   aesthetics apply to the bag and (except color) the median, and auxiliary
#'   aesthetics like `median.colour` are available for the remaining
#'   specifications. These override the internal defaults, which in turn
#'   override the standard values (as some auxiliaries, e.g. `outlier.shape`,
#'   are not provided separate defaults). Pass `sync()` to synchronize an
#'   auxiliary aesthetic with its standard counterpart.
#'
#'   **WARNING:**
#'   The trade-off between precision and runtime is greater for depth estimation
#'   than for density estimation. At the resolution of the default \eqn{100
#'   \times 100} grid, basic examples may vary noticeably when starting from
#'   different random seeds.
#' 

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_bagplot()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**, **`y`**
#' - **`component`**
#' - `linewidth`
#' - `linetype`
#' - `colour`
#' - `fill`
#' - `alpha`
#' - `shape`
#' - `stroke`
#' - `size`
#' - `group`
#' 

#' @template param-tick
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-bagplot.r
#' @export
geom_bagplot <- function(
    mapping = NULL, data = NULL, stat = "bagplot", position = "identity",
    ...,
    # NB: Defaults declared here will be missed by `layer(geom = "bagplot")` and
    # `stat_bagplot()`; they must be coordinated with the internal `*_defaults`
    # lists.
    median.colour = sync(), median.color = NULL,
    median.fill = "white", median.alpha = NA,
    fence.linewidth = 0.25, fence.linetype = 3L,
    fence.colour = NULL, fence.color = NULL,
    fence.fill = NULL, fence.alpha = 0.25,
    outlier.shape = NULL, outlier.stroke = 0.5, outlier.size = 1.5,
    outlier.colour = NULL, outlier.color = NULL,
    outlier.fill = NA, outlier.alpha = NA,
    na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE
) {
  
  median_gp <- list(
    colour = median.color %||% median.colour,
    fill   = median.fill,
    alpha  = median.alpha
  )
  
  fence_gp <- list(
    linewidth  = fence.linewidth,
    linetype   = fence.linetype,
    colour     = fence.color %||% fence.colour,
    fill       = fence.fill,
    alpha      = fence.alpha
  )
  
  outlier_gp <- list(
    shape  = outlier.shape,
    stroke = outlier.stroke,
    size   = outlier.size,
    colour = outlier.color %||% outlier.colour,
    fill   = outlier.fill,
    alpha  = outlier.alpha
  )
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBagplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      median_gp  = median_gp,
      fence_gp   = fence_gp,
      outlier_gp = outlier_gp,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBagplot <- ggproto(
  "GeomBagplot", GeomPolygon,
  
  required_aes = c("x", "y", "component"),
  
  default_aes = aes(
    # bag & median
    linewidth = 0.5, linetype = 1,
    shape = 21L, stroke = 0.5, size = 5,
    colour = "black", fill = "grey55", alpha = NA
  ),
  
  draw_panel = function(
    data, panel_params, coord,
    median_gp = NULL, fence_gp = NULL, outlier_gp = NULL,
    na.rm = FALSE
  ) {
    # save(data, panel_params, coord,
    #      median_gp, fence_gp, outlier_gp,
    #      na.rm,
    #      file = "geom-bagplot-draw-panel.rda")
    # load("geom-bagplot-draw-panel.rda")
    
    # initialize grob list; append in z-stack order
    grobs <- list()
    
    # fence data
    if (nrow(fence_data <- subset(data, component == "fence")) > 0L) {
      
      # default aesthetics (if not to coordinate with bag)
      fence_defaults <- list(
        linewidth = 0.25,
        linetype = 3L,
        alpha = 0.25
      )
      # specify independent aesthetics
      fence_aes <- GeomPolygon$aesthetics()
      for (aes_name in fence_aes) {
        fence_name <- paste0("fence.", aes_name)
        fence_data[[aes_name]] <- 
          (if (is.sync(fence_gp[[aes_name]])) 
            fence_data[[aes_name]]) %||%
          fence_gp[[aes_name]] %||%
          fence_defaults[[aes_name]] %||% 
          fence_data[[aes_name]]
      }
      fence_aes <- intersect(fence_aes, names(fence_data))
      fence_data <- subset(fence_data, select = fence_aes)
      
      # fence polygon grob
      grobs <- c(grobs, list(GeomPolygon$draw_panel(
        data = fence_data, panel_params = panel_params, coord = coord
      )))
    }
    
    # bag data
    bag_data <- subset(data, component == "bag")
    
    # bag polygon grob
    grobs <- c(grobs, list(GeomPolygon$draw_panel(
      data = bag_data, panel_params = panel_params, coord = coord
    )))
    
    # median data
    if (nrow(median_data <- subset(data, component == "median")) > 0L) {
      
      # default aesthetics (if not to coordinate with bag)
      median_defaults <- list(
        colour = sync(),
        fill = "white",
        alpha = NA
      )
      # specify independent aesthetics
      median_aes <- GeomPoint$aesthetics()
      for (aes_name in median_aes) {
        median_name <- paste0("median.", aes_name)
        median_data[[aes_name]] <- 
          (if (is.sync(median_gp[[aes_name]])) 
            median_data[[aes_name]]) %||%
          median_gp[[aes_name]] %||% 
          median_defaults[[aes_name]] %||% 
          median_data[[aes_name]]
      }
      median_aes <- intersect(median_aes, names(median_data))
      median_data <- subset(median_data, select = median_aes)
      
      # median point grob
      grobs <- c(grobs, list(GeomPoint$draw_panel(
        data = median_data, panel_params = panel_params, coord = coord,
        na.rm = na.rm
      )))
    }
    
    # outliers data
    if (nrow(outlier_data <- subset(data, component == "outliers")) > 0L) {
      
      # default aesthetics (if not to coordinate with bag and median)
      outlier_defaults <- list(
        stroke = 0.5,
        size = 1.5,
        fill = NA,
        alpha = NA
      )
      # specify independent aesthetics
      outlier_aes <- GeomPoint$aesthetics()
      for (aes_name in outlier_aes) {
        outlier_name <- paste0("outlier.", aes_name)
        outlier_data[[aes_name]] <- 
          (if (is.sync(outlier_gp[[aes_name]])) 
            outlier_data[[aes_name]]) %||%
          outlier_gp[[aes_name]] %||% 
          outlier_defaults[[aes_name]] %||% 
          outlier_data[[aes_name]]
      }
      outlier_aes <- intersect(outlier_aes, names(outlier_data))
      outlier_data <- subset(outlier_data, select = outlier_aes)
      
      # outliers point grob
      grobs <- c(grobs, list(GeomPoint$draw_panel(
        data = outlier_data, panel_params = panel_params, coord = coord,
        na.rm = na.rm
      )))
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_bagplot")
    grob
  }
)
