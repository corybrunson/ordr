#' @title Tidy `cancor()` list output
#'
#' @description These tidiers handle the output of `cancor()`, which is
#'   effectively an S3 object without a class attribute. It allows **ordr** to
#'   enhance the [list_tidiers] provided by **broom**.
#'
#' @details
#'
#' [cancor()] returns a named list of 5 elements. These tidiers rely on this
#' list structure to organize the model output into a tibble.
#'
#' @name cancor_tidiers
#' @aliases tidy.cancor
#' @param x A list with components `cor`, `xcoef`, `ycoef`, `xcenter`, and
#'   `ycenter` as returned by [stats::cancor()].
#' @param matrix Character specifying which list element should be tidied,
#'   matched to the following options.
#'
#'   - `"xcoef"`: returns information about the estimated coefficients in the
#'   `x` variables.
#'
#'   - `"ycoef"`: returns information about the estimated coefficients in the
#'   `y` variables
#'
#'   - `"cor"`: returns information about the canonical correlations.
#' @param ... Additional arguments allowed by generics; currently ignored.
#' @template return-tidier
#' @example inst/examples/ex-list-cancor-tidiers-savings.r
#' @family list tidiers
#' @seealso [generics::tidy()] [stats::cancor()]
NULL

#' @rdname cancor_tidiers
tidy_cancor <- function(x, matrix = "xcoef", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  matrix <- match.arg(matrix, c("xcoef", "ycoef", "cor"))
  
  # following `broom:::tidy_svd()`
  if (matrix == "xcoef") {
    res <- tibble::rownames_to_column(as.data.frame(x$xcoef), "variable")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("variable")),
      names_to = "CC",
      values_to = "value"
    )
    res$CC <- as.integer(stringr::str_remove(res$CC, "V"))
    if (! is.null(rownames(x$xcoef)))
      res$variable <- factor(res$variable, levels = rownames(x$xcoef))
    res <- res[order(res$CC, res$variable), c("CC", "variable", "value")]
  } else if (matrix == "ycoef") {
    res <- tibble::rownames_to_column(as.data.frame(x$ycoef), "variable")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("variable")),
      names_to = "CC",
      values_to = "value"
    )
    res$CC <- as.integer(stringr::str_remove(res$CC, "V"))
    if (! is.null(rownames(x$ycoef)))
      res$variable <- factor(res$variable, levels = rownames(x$ycoef))
    res <- res[order(res$CC, res$variable), c("CC", "variable", "value")]
  } else if (matrix == "cor") {
    res <- data.frame(
      CC = seq_along(x$cor),
      cor = x$cor
    )
    res$percent <- res$cor^2 / sum(res$cor^2)
    res$cumulative <- cumsum(res$percent)
  }
  
  as_tibble(res)
}
