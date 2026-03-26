
#' @importFrom utils getFromNamespace

#' @importFrom rlang list2

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

as_tbl_ord_default <- function(x) {
  class(x) <- c("tbl_ord", class(x))
  x
}
