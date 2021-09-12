
#' | Original function                  | Hide params | Add names | New class |
#' | :--------------------------------- | :---------- | :-------- | :-------- |
#' | [stats::cancor()]                  | No          | No        | Yes       |

#' @importFrom stats cancor
#' @inheritParams stats::cancor

cancor_ord <- function(x, y, xcenter = TRUE, ycenter = TRUE) {
  res <- stats::cancor(x, y, xcenter = xcenter, ycenter = ycenter)
  class(res) <- "cancor_ord"
  res
}
