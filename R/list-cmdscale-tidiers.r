#' @title Tidy `cmdscale()` list output
#'
#' @description These tidiers handle the output of `cmdscale()`, which under
#'   certain conditions is effectively an S3 object without a class attribute.
#'   It allows **ordr** to enhance the [list_tidiers] provided by **broom**.
#' 
#' @details
#' 
#' When [cmdscale()] is instructed to return any of several optional elements,
#' or when `list. = TRUE`, the output is not the default point coordinate matrix
#' but a 5-element list with a consistent naming scheme (though some elements
#' will be `NULL` if their parameters are not set to `TRUE`). These tidiers rely
#' on this list structure to organize the model output into a tibble.
#' 
#' @name cmdscale_tidiers
#' @aliases tidy.cmdscale glance.cmdscale
#' @param x A list with components `points`, `eig`, `x`, `ac`, and `GOF` as
#'   returned by [stats::cmdscale()].
#' @param matrix Character specifying which list element should be tidied,
#'   matched to the following options.
#'
#'   - `"points"`: returns information about the coordinates in the
#'   representation space.
#'
#'   - `"x"`: returns information about the doubly-centered symmetric matrix
#'   used in the calculation.
#'
#'   - `"eig"`: returns information about the eigenvalues.
#' @param ... Additional arguments allowed by generics; currently ignored.
#' @template return-tidier
#' @example inst/examples/ex-list-cmdscale-tidiers-cities.r
#' @family list tidiers
#' @seealso [generics::tidy()] [generics::glance()] [stats::cmdscale()]
NULL

#' @rdname cmdscale_tidiers
tidy_cmdscale <- function(x, matrix = "points", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  matrix <- match.arg(matrix, c("points", "eig", "x"))
  
  if (matrix == "points") {
    res <- as.data.frame(x$points)
    names(res) <- paste0("PCo", seq(ncol(res)))
    res <- tibble::rownames_to_column(res, "point")
  } else if (matrix == "x") {
    if (is.null(x$x)) stop("Matrix `x` is NULL.")
    res <- as.data.frame(x$x)
    names(res) <- seq(ncol(res))
    res <- tibble::rownames_to_column(res, "point1")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("point1")),
      names_to = "point2", values_to = "DC"
    )
    res <- mutate(res, dplyr::across(c("point1", "point2"), as.integer))
    res <- res[res$point1 < res$point2, ]
    res <- mutate(res, dplyr::across(
      c("point1", "point2"),
      ~ rownames(x$points)[.]
    ))
  } else if (matrix == "eig") {
    ks <- seq(ncol(x$points))
    res <- data.frame(
      PCo = seq_along(x$eig)[ks],
      eig = x$eig[ks]
    )
    # following `broom:::tidy_svd()`
    # for each artificial coordinate, eigenvalue = variance
    res$percent = res$eig / sum(res$eig)
    res$cumulative = cumsum(res$percent)
  }
  as_tibble(res)
}

#' @rdname cmdscale_tidiers
glance_cmdscale <- function(x, ...) {
  as_glance_tibble(
    n = nrow(x$points),
    k = ncol(x$points),
    ac = x$ac,
    GOF1 = x$GOF[[1L]],
    GOF2 = x$GOF[[2L]],
    na_types = "iirrr"
  )
}
