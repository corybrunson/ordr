#' @title Tidy an 'eigen' object
#'
#' @description This [broom::tidy()] method tidies either of the two outputs of
#'   [base::eigen()], depending on the `matrix` parameter, analogously to
#'   [broom::tidy_svd()].
#'
#' @name methods-eigen
#' @param x An 'eigen' object, as returned by [base::eigen()].
#' @param matrix Character specifying which component of the eigendecomposition
#'   should be tidied.
#'
#'   - `"Lambda"`, `"vectors"`: returns information about the eigenvectors of
#'   `x`
#'   
#'   - `"lambda"`, `"values"`: returns information about the eigenvalues
#'   
#' @example inst/examples/ex-broom-tidy-eigen.r
tidy.eigen <- function(x, matrix = "vectors", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  matrix <- match.arg(matrix, c("Lambda", "vectors", "lambda", "values"))
  
  if (matrix %in% c("Lambda", "vectors")) {
    res <- tibble::as_tibble(x$vectors, .name_repair = "unique")
    res <- tibble::rowid_to_column(res, "row")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -row),
      names_to = "EV",
      values_to = "value"
    )
    res <- dplyr::mutate(
      res,
      EV = as.integer(stringr::str_remove(EV, "..."))
    )
    res <- dplyr::arrange(res, EV, row)
    res <- as.data.frame(res)
  } else if (matrix %in% c("lambda", "values")) {
    res <- tibble::tibble(EV = seq_along(x$values), inertia = x$values)
    res <- dplyr::mutate(res, prop_var = inertia / sum(inertia))
  }
  tibble::as_tibble(res)
}
