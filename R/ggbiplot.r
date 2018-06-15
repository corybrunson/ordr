#' Build a biplot object from ordination data wrapped as a bibble
#' 

#' \code{ggbiplot} produces a \strong{\link[ggplot2]{ggplot2}} object from a 
#' \code{"bbl"}-class ordination object. The baseline object is the default 
#' unadorned \code{\link[ggplot2]{ggplot}} object \code{p} with the following
#' differences:
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

#' @import ggplot2

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  ...
) {
  mapping <- ordinate_aes(ordination, mapping)
  
  p <- ggplot(
    data = fortify(ordination, include = "all"),
    mapping = mapping,
    environment = parent.frame(),
    ...
  )
  p$mapping <- c(p$mapping, aes(.matrix = .matrix))
  p$coordinates <- coord_fixed()
  class(p) <- c("ggbiplot", class(p))
  
  p
}

# interpret numerical x and y coordinates as coordinates;
# assume first two coordinates if none are provided
ordinate_aes <- function(ordination, mapping) {
  coords <- get_coord(ordination)
  coord_vars <- syms(coords)
  if (is.null(mapping$y)) {
    mapping <- c(aes(y = !!coord_vars[[2]]), mapping)
  } else {
    if (is.numeric(mapping$y) && length(mapping$y) == 1) {
      mapping <- c(
        aes(y = !!coord_vars[[mapping$y]]),
        mapping[setdiff(names(mapping), "y")]
      )
    }
  }
  if (is.null(mapping$x)) {
    mapping <- c(aes(x = !!coord_vars[[1]]), mapping)
  } else {
    if (is.numeric(mapping$x) && length(mapping$x) == 1) {
      mapping <- c(
        aes(x = !!coord_vars[[mapping$x]]),
        mapping[setdiff(names(mapping), "x")]
      )
    }
  }
  class(mapping) <- "uneval"
  mapping
}
