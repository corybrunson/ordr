
deprecate_for_gggda <- function(ggproto) {
  stopifnot(inherits(get(ggproto), "ggproto"))
  rlang::inform(
    paste0("The ggproto `", ggproto, "` will soon be migrated to {gggda}."),
    .frequency = "once", .frequency_id = ggproto
  )
}

#' @importFrom utils getFromNamespace

#' @importFrom rlang list2

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

as_tbl_ord_default <- function(x) {
  class(x) <- c("tbl_ord", class(x))
  x
}
