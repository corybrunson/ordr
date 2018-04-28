
# allow coordinates to be pulled? only one at a time?
pull.bbl <- function(x, matrix = NULL, var = -1) {
  x <- to_bibble(x)
  var <- enquo(var)
  if (is.null(matrix)) {
    return(unlist(lapply(c("u", "v"), function(matrix) {
      d <- get_uv(x, matrix = matrix)
      p_tmp <- try(pull(d, !!var))
      if (class(p_tmp)[1] == "try-error") return(c())
      p_tmp
    })))
  } else if (matrix == "uv") {
    return(unlist(lapply(c("u", "v"), function(matrix) {
      pull(get_uv(x, matrix = matrix), !!var)
    })))
  } else {
    return(pull(get_uv(x, matrix = matrix), !!var))
  }
}

pull_u <- function(x, var = -1) {
  stopifnot(class(x)[1] == "bbl")
  pull.bbl(x, matrix = "u", var = var)
}

pull_v <- function(x, var = -1) {
  stopifnot(class(x)[1] == "bbl")
  pull.bbl(x, matrix = "v", var = var)
}
