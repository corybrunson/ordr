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
  coords <- syms(get_coord(ordination))
  if (is.null(mapping$y)) {
    mapping <- c(aes(y = !!coords[[2]]), mapping)
  } else {
    if (is.numeric(mapping$y)) {
      mapping <- c(
        aes(y = !!coords[[mapping$y]]),
        mapping[setdiff(names(mapping), "y")]
      )
    }
  }
  if (is.null(mapping$x)) {
    mapping <- c(aes(x = !!coords[[1]]), mapping)
  } else {
    if (is.numeric(mapping$x)) {
      mapping <- c(
        aes(x = !!coords[[mapping$x]]),
        mapping[setdiff(names(mapping), "x")]
      )
    }
  }
  class(mapping) <- "uneval"
  mapping
}
