#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(),
  ...
) {
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
