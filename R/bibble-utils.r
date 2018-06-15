
bibble_factors <- c(
  u = "u", v = "v", uv = "uv",
  U = "u", V = "v", UV = "uv",
  left = "u", right = "v", both = "uv",
  subjects = "u", variables = "v"
)
match_factor <- function(x) {
  x <- match.arg(x, names(bibble_factors))
  unname(bibble_factors[x])
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
