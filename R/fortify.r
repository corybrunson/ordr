
#' @rdname ggbiplot
#' @param inertia How to pass inertia to the row and column coordinates. Numeric
#'   values must be between \code{0} and \code{1}. A numeric vector 
#'   \eqn{p=(p_1,p_2)} of length two will scale the standard row coordinates by 
#'   \eqn{S^{p_1}} and the standard column coordinates by \eqn{S^{p_2}}. A 
#'   numeric scalar \eqn{p} will be interpreted as \eqn{(p,1-p)}. Also accepts 
#'   the character strings \code{"standard"} (equivalent to \code{c(0, 0)}); 
#'   \code{"u"}, \code{"rows"}, and \code{"left"} (equivalent to \code{c(1,
#'   0)}); \code{"v"}, \code{"columns"}, and \code{"right"} (equivalent to
#'   \code{c(0, 1)}); and \code{"principal"} and \code{"symmetric"} (equivalent
#'   to \code{c(.5, .5)}).
#' @export
fortify.bbl <- function(
  bp,
  inertia = c(1, 0),
  matrix.names = c("u", "v"), include = "shared"
) {
  
  u <- cbind(get_u(bp), .matrix = matrix.names[1])
  v <- cbind(get_v(bp), .matrix = matrix.names[2])
  res <- switch(
    match.arg(include, c("coordinates", "shared", "all")),
    coordinates = {
      coord <- get_coordinates(bp)$.name
      as_tibble(as.data.frame(rbind(u[coord], v[coord])))
    },
    shared = {
      int <- intersect(names(u), names(v))
      as_tibble(as.data.frame(rbind(u[int], v[int])))
    },
    all = as_tibble(as.data.frame(dplyr::bind_rows(u, v)))
  )
  res$.matrix <- factor(res$.matrix, levels = matrix.names)
  res
}
