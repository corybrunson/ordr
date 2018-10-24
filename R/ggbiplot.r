#' Biplots following the grammar of graphics
#' 
#' Build a biplot visualization from ordination data wrapped as a
#' \code{tbl_ord}.
#' 

#' \code{ggbiplot} produces a \strong{\link[ggplot2]{ggplot2}} object from a 
#' \code{\link{tbl_ord}} object. The baseline object is the default unadorned
#' \code{ggplot} object \code{p} with the following differences:

#' \itemize{

#'   \item \code{p$mapping} is augmented with \code{.matrix = .matrix}, which
#'   expects either \code{.matrix = "u"} or \code{.matrix = "v"} from the biplot
#'   layers.

#'   \item \code{p$coordinates} is set to \code{\link[ggplot2]{coord_fixed}} in
#'   order to faithfully render the geometry of an ordination.

#'   \item \code{p} is assigned the class \code{"ggbiplot"}. This serves no
#'   functional purpose currently.

#' }

#' Furthermore, the user may feed single integer values to the \code{x} and 
#' \code{y} aesthetics, which will be interpreted as the corresponding
#' coordinates in the ordination.
#' 

#' @template ggbiplot-layers

#' @name ggbiplot
#' @import ggplot2
#' @param ordination A \code{\link{tbl_ord}}.
#' @param mapping List of default aesthetic mappings to use for the biplot. The 
#'   default assigns the first two coordinates to the aesthetics \code{x} and 
#'   \code{y}. Other assignments must be supplied in each layer added to the 
#'   plot.
#' @param sec.axes Matrix factor character to specify a secondary set of axes.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  sec.axes = NULL, scale.factor = NULL,
  ...
) {
  # fortify `ordination` if necessary
  ordination <- fortify(ordination, include = "all")
  
  # augment `mapping`, if necessary, with default coordinates
  mapping <- ordinate_aes(ordination, mapping)
  
  # if `sec.axes` is specified, then fortify `ordination` and scale the
  # secondary axis coordinates to match the primary axis
  if (! is.null(sec.axes)) {
    
    sec.axes <- match_factor(sec.axes)
    if (! sec.axes %in% c("u", "v")) {
      stop("Select one matrix factor, 'u' or 'v', to scale to secondary axes.")
    }
    pri.axes <- setdiff(c("u", "v"), sec.axes)
    
    .coords <- str_sub(as.character(mapping[c("x", "y")]), start = 2)
    
    if (is.null(scale.factor)) {
      ps_lim <- lapply(c(pri.axes, sec.axes), function(.m) {
        apply(
          ordination[ordination$.matrix == .m, .coords],
          2, function(x) c(min(x), max(x))
        )
      })
      scale.factor <- min(ps_lim[[1]] / ps_lim[[2]])
    }
    
    ordination <- mutate_at(
      ordination,
      vars(.coords),
      funs(ifelse(.matrix == sec.axes, . * scale.factor, .))
    )
    
  }
  
  # conventional `ggplot()` call
  p <- ggplot(
    data = ordination,
    mapping = mapping,
    environment = parent.frame(),
    ...
  )
  # .matrix aesthetic indicating whether to plot cases or variables
  .matrix_aes <- list(.matrix = rlang::quo(!! rlang::sym(".matrix")))
  class(.matrix_aes) <- "uneval"
  p$mapping <- c(p$mapping, .matrix_aes)
  
  # if `sec.axes` is specified, then add secondary axes
  if (! is.null(sec.axes)) {
    # THIS APPROACH IS VULNERABLE TO DOWNSTREAM `x` AND `y` SCALES
    p <- p + scale_x_continuous(sec.axis = sec_axis(~ . / scale.factor))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~ . / scale.factor))
  }
  
  # synchronize the scales AND BREAKS of the axes
  p$coordinates <- coord_fixed()
  
  # add class label for potential future use
  class(p) <- c("ggbiplot", class(p))
  
  p
}

# interpret numerical x and y coordinates as coordinates;
# assume first two coordinates if none are provided
ordinate_aes <- function(ordination, mapping) {
  coords <- get_coord(ordination, align = TRUE)
  coord_vars <- syms(coords)
  if (is.null(mapping$y)) {
    mapping <- c(aes(y = !! coord_vars[[2]]), mapping)
  } else {
    if (is.numeric(mapping$y) && length(mapping$y) == 1) {
      mapping <- c(
        aes(y = !! coord_vars[[mapping$y]]),
        mapping[setdiff(names(mapping), "y")]
      )
    }
  }
  if (is.null(mapping$x)) {
    mapping <- c(aes(x = !! coord_vars[[1]]), mapping)
  } else {
    if (is.numeric(mapping$x) && length(mapping$x) == 1) {
      mapping <- c(
        aes(x = !! coord_vars[[mapping$x]]),
        mapping[setdiff(names(mapping), "x")]
      )
    }
  }
  class(mapping) <- "uneval"
  mapping
}
