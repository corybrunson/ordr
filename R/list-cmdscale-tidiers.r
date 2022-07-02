#' @title Tidy `cmdscale()` list output
#'
#' @param x A list with components `points`, `eig`, `x`, `ac`, and `GOF`
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
#'
#' @example inst/examples/ex-list-cmdscale-tidiers-cities.r

#' @name cmdscale_tidiers
#' @aliases tidy.cmdscale glance.cmdscale
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
    GOF2 = x$GOF[[2L]]
  )
}

as_glance_tibble <- getFromNamespace("as_glance_tibble", "broom")
