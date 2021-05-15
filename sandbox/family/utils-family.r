
family_arg <- function(family_fun) {
  if (! is.null(family_fun)) {
    if (is.character(family_fun)) {
      family_fun <- get(family_fun, mode = "function", envir = parent.frame())
    }
    if (is.function(family_fun)) {
      family_fun <- family_fun()
    }
  }
  family_fun
}
