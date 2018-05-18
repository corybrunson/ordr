#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(),
  ...
) {
  gg <- ggplot(
    data = fortify(ordination, include = "all"),
    mapping = mapping,
    environment = parent.frame(),
    ...
  ) + coord_fixed()
  gg$mapping <- c(gg$mapping, aes(.matrix = .matrix))
  class(gg) <- c("ggbiplot", class(gg))
  gg
}
