
# biplot data for which dimensions are not (meaningfully) signed
alignable <- c(
  "cmds",
  "prcomp"
)

# align a biplot's coordinates to those of another biplot
align_with <- function(x, matrix, y, by = NULL) {
  as.list(bibble_factors)[[match.arg(matrix, names(bibble_factors))]]
  if (matrix == "uv") stop("No alignment method for both matrix factors.")
  prev_class <- setdiff(c(attr(x, "preclass"), class(x)), "bbl")[1]
  if (!(prev_class %in% alignable)) {
    stop("Coordinates of class-'", prev_class, "' objects cannot be realigned.")
  }
  x <- to_bibble(x)
  # shared coordinates
  x_coords <- get_coordinates(x)$.name
  y_coords <- get_coordinates(y)$.name
  int_coords <- intersect(x_coords, y_coords)
  if (length(int_coords) == 0) {
    warning("`x` and `y` share no named coordinates,",
            "so their coordinates will be matched in order.")
    coords <- int_coords
  } else {
    coords <- 1:min(length(x_coords), length(y_coords))
  }
  # shared observations/variables
  x_df <- get_uv(x, matrix)
  y_df <- get_uv(y, matrix)
  if (is.null(by)) {
    x_i <- y_i <- 1:min(nrow(x_df), nrow(y_df))
  } else {
    x_j <- mutate(x_df, id = row_number())
    y_j <- mutate(y_df, id = row_number())
    x_i <- pull(left_join(y_df, x_j, by = by), id)
    y_i <- pull(left_join(x_df, y_j, by = by), id)
  }
  # signs of dot products of shared coordinates at shared obs/vars
  signs <- sign(apply(
    (factor_uv(x, matrix)[x_i, coords] * factor_uv(y, matrix)[y_i, coords]),
    2, sum
  ))
  # reverse misaligned directions
  x[["u"]][, coords] <- sweep(x[["u"]][, coords], 2, signs, "/")
  x[["v"]][, coords] <- sweep(x[["v"]][, coords], 2, signs, "/")
  x
}
