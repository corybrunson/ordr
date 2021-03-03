
#' @importFrom graphics plot
#' @importFrom stats biplot

#' @method biplot tbl_ord
#' @export
biplot.tbl_ord <- function(x, choices = 1L:2L, ...) {
  # biplot method for original class
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("biplot"))) {
    class(x) <- prev_class
    return(biplot(x, ...))
  }
  # tbl_ord biplot method based on stats:::biplot.prcomp
  scores <- get_rows(x)
  loadings <- get_cols(x)
  if (length(choices) != 2L)
    stop("Length of choices must be 2.")
  if (! length(scores)) 
    stop(gettextf("Ordination '%s' has no scores.", deparse(substitute(x))),
         domain = NA)
  if (is.complex(scores))
    stop("Biplots are not defined for complex ordinations.")
  biplot.default(
    scores[, choices, drop = FALSE],
    loadings[, choices, drop = FALSE],
    ...
  )
  invisible()
}

#' @method plot tbl_ord
#' @export
plot.tbl_ord <- function(x, ...) {
  # use plot method for original class if available
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("plot"))) {
    class(x) <- prev_class
    return(plot(x, ...))
  }
  biplot(x, ...)
}

#' @importFrom utils getFromNamespace
biplot.default <- getFromNamespace("biplot.default", "stats")
