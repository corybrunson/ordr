#' @title Bagplots
#'
#' @description Render bagplots from tagged data comprising medians, hulls,
#'   contours, and outlier specifications.

#' @details `geom_bagplot()` is designed to pair with [stat_bagplot()].
#'
#' **WARNING:**
#'   The trade-off between precision and runtime is far greater for depth
#'   estimation than for density estimation. At the resolution of the default
#'   \eqn{100 \times 100} grid, basic examples may vary widely starting from
#'   different random seeds.
#' 

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_bagplot()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**, **`y`**
#' - **`component`**
#' - `linewidth`, `linetype`,
#'   `colour`, `fill`, `alpha`
#' - `shape`, `stroke`, `size`,
#'   `median_colour`, `median_fill`, `median_alpha`
#' - `fence_linewidth`, `fence_linetype`,
#'   `fence_colour`, `fence_fill`, `fence_alpha`
#' - `outlier_shape`, `outlier_stroke`, `outlier_size`,
#'   `outlier_colour`, `outlier_fill`, `outlier_alpha`
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
    na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBagplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
    # bag
    linewidth = 0.5, linetype = 1,
    colour = "black", fill = "grey55", alpha = NA,
    # median
    shape = 19L, stroke = 0.5, size = 5,
    median_colour = NULL, median_fill = NULL, median_alpha = NULL,
    # fence
    fence_linewidth = NULL, fence_linetype = NULL,
    fence_colour = NULL, fence_fill = NULL, fence_alpha = NULL,
    # outliers
    outlier_shape = NULL, outlier_stroke = NULL, outlier_size = NULL,
    outlier_colour = NULL, outlier_fill = NULL, outlier_alpha = NULL
  ),
  
  draw_panel = function(
    data, panel_params, coord,
    na.rm = FALSE
  ) {
    # save(data, panel_params, coord, na.rm,
    #      file = "geom-bagplot-draw-panel.rda")
    # load("geom-bagplot-draw-panel.rda")
    
    # initialize grob list; append in z-stack order
    grobs <- list()
    
    # fence data
    if (nrow(fence_data <- subset(data, component == "fence")) > 0L) {
      
      # default aesthetics (if not to coordinate with bag)
      fence_defaults <- list(
        linewidth = 0.25,
        linetype = 3,
        alpha = 0.25
      )
      
      # specify independent aesthetics
      fence_aes <- GeomPolygon$aesthetics()
      for (aes_name in fence_aes) {
        fence_name <- paste0("fence_", aes_name)
        fence_data[[aes_name]] <- 
          fence_data[[fence_name]] %||% 
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
        colour = "white",
        fill = NA,
        alpha = NA
      )
      
      # specify independent aesthetics
      median_aes <- GeomPoint$aesthetics()
      for (aes_name in median_aes) {
        median_name <- paste0("median_", aes_name)
        median_data[[aes_name]] <- 
          median_data[[median_name]] %||% 
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
    if (nrow(outliers_data <- subset(data, component == "outliers")) > 0L) {
      
      # default aesthetics (if not to coordinate with bag and median)
      outliers_defaults <- list(
        stroke = 0.5,
        size = 1.5,
        fill = NA,
        alpha = NA
      )
      
      # specify independent aesthetics
      outliers_aes <- GeomPoint$aesthetics()
      for (aes_name in outliers_aes) {
        outliers_name <- paste0("outlier_", aes_name)
        outliers_data[[aes_name]] <- 
          outliers_data[[outliers_name]] %||% 
          outliers_defaults[[aes_name]] %||% 
          outliers_data[[aes_name]]
      }
      outliers_aes <- intersect(outliers_aes, names(outliers_data))
      outliers_data <- subset(outliers_data, select = outliers_aes)
      
      # outliers point grob
      grobs <- c(grobs, list(GeomPoint$draw_panel(
        data = outliers_data, panel_params = panel_params, coord = coord,
        na.rm = na.rm
      )))
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_bagplot")
    grob
  }
)
