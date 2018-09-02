#' Build a biplot object from ordination data wrapped as a \code{tbl_ord}
#' 

#' \code{ggbiplot} produces a \strong{\link[ggplot2]{ggplot2}} object from a 
#' \code{\link{tbl_ord}} object. The baseline object is the default unadorned
#' \code{ggplot} object \code{p} with the following differences:
#' \itemize{
#'   \item \code{p$mapping} is augmented with \code{.matrix = .matrix},
#'         which expects either \code{.matrix = "u"} or \code{.matrix = "v"}
#'         from the biplot layers.
#'   \item \code{p$coordinates} is set to \code{\link[ggplot2]{coord_fixed}}
#'         in order to faithfully render the geometry of an ordination.
#'   \item \code{p} is assigned the class \code{"ggbiplot"}. This serves no
#'         functional purpose currently.
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
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  ...
) {
  mapping <- ordinate_aes(ordination, mapping)
  
  # conventional ggplot call, fortifying ordination if necessary
  p <- ggplot(
    data = fortify(ordination, include = "all"),
    mapping = mapping,
    environment = parent.frame(),
    ...
  )
  # .matrix aesthetic indicating whether to plot cases or variables
  .matrix_aes <- list(.matrix = rlang::quo(!!rlang::sym(".matrix")))
  class(.matrix_aes) <- "uneval"
  p$mapping <- c(p$mapping, .matrix_aes)
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
