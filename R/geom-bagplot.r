#' @title Bagplots
#'
#' @description Render bagplots from tagged data comprising medians, hulls,
#'   contours, and outlier specifications.

#' @details `geom_bagplot()` is designed to pair with [stat_bagplot()].
#' 

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_bagplot()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**, **`y`**
#' - **`component`**
#' - `linewidth`, `linetype`,
#'   `colour`, `fill`, `alpha`
#' - `median_shape`, `median_stroke`, `median_size`,
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
#' @inheritParams stat_bagplot
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-bagplot.r
#' @export
geom_bagplot <- function(
    mapping = NULL, data = NULL, stat = "bag", position = "identity",
    median = TRUE, fence = TRUE, outliers = TRUE,
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
      median = TRUE,
      fence = TRUE,
      outliers = TRUE,
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
    median_shape = 19L, median_stroke = 0.5, median_size = 5,
    median_colour = "white", median_fill = NA, median_alpha = NA,
    # fence
    fence_linewidth = 0, fence_linetype = 0,
    fence_colour = NA, fence_fill = "grey85", fence_alpha = 0.75,
    # outliers
    outlier_shape = 19L, outlier_stroke = 0.5, outlier_size = 1.5,
    outlier_colour = NA, outlier_fill = NA, outlier_alpha = NA
  ),
  
  draw_panel = function(
    data, panel_params, coord,
    median = TRUE, fence = TRUE, outliers = TRUE,
    na.rm = FALSE
  ) {
    # save(data, panel_params, coord, median, fence, outliers, na.rm,
    #      file = "geom-bagplot-draw-panel.rda")
    # load("geom-bagplot-draw-panel.rda")
    
    # initialize grob list; append in z-stack order
    grobs <- list()
    
    # fence data
    if (fence) {
      if (nrow(fence_data <- subset(data, component == "fence")) == 0L) {
        warning("`data` has no `\"fence\"` component.")
      } else {
        
        # specify independent aesthetics
        fence_aes <- GeomPolygon$aesthetics()
        for (aes_name in fence_aes) {
          fence_name <- paste0("fence_", aes_name)
          if (! is.null(data[[fence_name]])) {
            fence_data[[aes_name]] <- fence_data[[fence_name]]
          }
        }
        fence_aes <- intersect(fence_aes, names(fence_data))
        fence_data <- subset(fence_data, select = fence_aes)
        
        # fence polygon grob
        grobs <- c(grobs, list(GeomPolygon$draw_panel(
          data = fence_data, panel_params = panel_params, coord = coord
        )))
      }
    }
    
    # bag data
    bag_data <- subset(data, component == "bag")
    
    # bag polygon grob
    grobs <- c(grobs, list(GeomPolygon$draw_panel(
      data = bag_data, panel_params = panel_params, coord = coord
    )))
    
    # median data
    if (median) {
      if (nrow(median_data <- subset(data, component == "median")) == 0L) {
        warning("`data` has no `\"median\"` component.")
      } else {
        
        # specify independent aesthetics
        median_aes <- GeomPoint$aesthetics()
        for (aes_name in median_aes) {
          median_name <- paste0("median_", aes_name)
          if (! is.null(data[[median_name]])) {
            median_data[[aes_name]] <- median_data[[median_name]]
          }
        }
        median_aes <- intersect(median_aes, names(median_data))
        median_data <- subset(median_data, select = median_aes)
        
        # median point grob
        grobs <- c(grobs, list(GeomPoint$draw_panel(
          data = median_data, panel_params = panel_params, coord = coord,
          na.rm = na.rm
        )))
      }
    }
    
    # outlier data
    if (outliers) {
      if (nrow(outliers_data <- subset(data, component == "outliers")) == 0L) {
        # warning("`data` has no `\"outliers\"` component.")
      } else {
        
        # specify independent aesthetics
        outliers_aes <- GeomPoint$aesthetics()
        for (aes_name in outliers_aes) {
          outliers_name <- paste0("outliers_", aes_name)
          if (! is.null(data[[outliers_name]])) {
            outliers_data[[aes_name]] <- outliers_data[[outliers_name]]
          }
        }
        outliers_aes <- intersect(outliers_aes, names(outliers_data))
        outliers_data <- subset(outliers_data, select = outliers_aes)
        
        # outliers point grob
        grobs <- c(grobs, list(GeomPoint$draw_panel(
          data = outliers_data, panel_params = panel_params, coord = coord,
          na.rm = na.rm
        )))
      }
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_bagplot")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)
