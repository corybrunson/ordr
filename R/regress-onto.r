
# check that 'newdata' is compatible with u (or v if so specified)
compatible_v <- function(bbl, newdata) {
  if (ncol(newdata) != nrow(get_v(bbl))) return(FALSE)
  if (!is.null(colnames(newdata)) && any(colnames(newdata) != get_v(bbl)$name))
    return(FALSE)
  TRUE
}
compatible_u <- function(bbl, newdata) {
  if (nrow(newdata) != nrow(get_u(bbl))) return(FALSE)
  if (!is.null(rownames(newdata)) && any(rownames(newdata) != get_u(bbl)$name))
    return(FALSE)
  TRUE
}

# regress 'newdat' on the appropriate factor of 'bbl'
regress_onto <- function(bbl, newdata, factor = "v") {
  compatible_fun <- switch(factor, u = compatible_u, v = compatible_v)
  stopifnot(compatible_fun(bbl, newdata))
  get_fun <- switch(factor, u = get_u, v = get_v)
  get_fun2 <- switch(factor, u = get_v, v = get_u)
  factor_fun <- switch(factor, u = factor_u, v = factor_v)
  fit <- lm(newdata ~ factor_fun(bbl))
  assign(factor, bind_rows(
    mutate(get_fun(bbl), .source = ".original"),
    mutate(get_fun(fit), .source = ".regressed")
  ))
  assign(setdiff(c("u", "v"), factor), get_fun2(bbl))
  make_bibble(
    u = u,
    v = v,
    coordinates = inner_join(
      get_coordinates(bbl),
      get_coordinates(fit),
      by = ".name", suffix = c(".original", ".regressed")
    )
  )
}
