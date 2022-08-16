#' @title Plot and biplot methods for 'tbl_ord' objects
#'
#' @description Adapt **stats** 'prcomp' and 'princomp' methods for `plot()`,
#'   `screeplot()`, and `biplot()` generics to 'tbl_ord' objects.
#'
#' @details
#'
#' These methods defer to any `plot()` and `biplot()` methods for the original,
#' underlying model classes of 'tbl_ord' objects. If none are found: Following
#' the examples of [stats::plot.prcomp()] and [stats::plot.princomp()],
#' `plot.tbl_ord()` calls on [stats::screeplot()] to produce a scree plot of the
#' decomposition of variance in the singular value decomposition. Similarly
#' following [stats::biplot.prcomp()] and [stats::biplot.princomp()],
#' `biplot.tbl_ord()` produces a biplot of both rows and columns, using text
#' labels when available and markers otherwise, with rows and columns
#' distinguished by color and no additional annotation (e.g. vectors). The
#' biplot confers inertia according to [get_conference()] unless the proportions
#' do not sum to 1, in which case it produces a symmetric biplot (inertia
#' conferred equally to rows and columns).
#'
#' @include ord-tbl.r
#' @importFrom graphics plot
#' @importFrom stats screeplot biplot
#' @param x A 'tbl_ord' object.
#' @param main A main title for the plot, passed to other methods (included to
#'   enable parsing of object name).
#' @param ... Additional arguments passed to other methods.
#' @return Nothing, but a plot is produced on the current graphics device.
#' @example inst/examples/ex-plot.r

#' @method plot tbl_ord
#' @export
plot.tbl_ord <- function(x, main = deparse(substitute(x)), ...) {
  force(main)
  # use `plot()` method for original class if available
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("plot"))) {
    class(x) <- prev_class
    return(plot(x, main = main, ...))
  }
  screeplot(x, main = main, ...)
}

#' @method screeplot tbl_ord
#' @export
screeplot.tbl_ord <- function(x, main = deparse(substitute(x)), ...) {
  force(main)
  # use `screeplot()` method for original class if available
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("screeplot"))) {
    class(x) <- prev_class
    return(screeplot(x, main = main, ...))
  }
  # label axis columns
  sdev <- sqrt(recover_inertia(x))
  names(sdev) <- recover_coord(x)
  screeplot.default(x = list(sdev = sdev), main = main, ...)
}

#' @method biplot tbl_ord
#' @export
biplot.tbl_ord <- function(x, main = deparse(substitute(x)), ...) {
  force(main)
  # use `biplot()` method for original class if available
  prev_class <- setdiff(class(x), "tbl_ord")
  if (any(prev_class %in% method_classes("biplot"))) {
    class(x) <- prev_class
    return(biplot(x, main = main, ...))
  }
  # if default conference does not support a biplot interpretation, then confer
  # inertia symmetrically
  if (! is.null(recover_conference(x)) &&
      sum(recover_conference(x)) != 1 &&
      is.null(attr(x, "confer")))
    x <- confer_inertia(x, p = .5)
  biplot.default(
    x = get_rows(x), y = get_cols(x),
    main = main, ...
  )
}

screeplot.default <- getFromNamespace("screeplot.default", "stats")
biplot.default <- getFromNamespace("biplot.default", "stats")
