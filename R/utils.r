
as_tbl_ord_default <- function(x) {
  class(x) <- c("tbl_ord", class(x))
  x
}

tbl_ord_factors <- c(
  u = "u", v = "v", uv = "uv",
  U = "u", V = "v", UV = "uv",
  left = "u", right = "v", both = "uv",
  cases = "u", variables = "v",
  subjects = "u", measures = "v",
  scores = "u", loadings = "v"
)
match_factor <- function(x) {
  x <- match.arg(x, names(tbl_ord_factors))
  unname(tbl_ord_factors[x])
}

method_classes <- function(generic.function) {
  stringr::str_replace(
    stringr::str_subset(
      rownames(attr(utils::methods(generic.function), "info")),
      paste0("^", generic.function, "\\.")
    ),
    paste0("^", generic.function, "\\."), ""
  )
}

tibble_pole <- function(nrow) {
  as_tibble(matrix(nrow = nrow, ncol = 0))
}
