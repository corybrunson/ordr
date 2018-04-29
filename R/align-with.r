
# biplot data for which dimensions are not (meaningfully) ordered
unordered_classes <- c(
  # multidimensional scaling / principal coordinates analysis
  "cmds"
)

# biplot data for which dimensions are not (meaningfully) signed
unsigned_classes <- c(
  # multidimensional scaling / principal coordinates analysis
  "cmds",
  # principal components analysis
  "prcomp"
)

# align a biplot's coordinates to those of another biplot
align_with <- function(x, matrix, y, by = NULL) {
  matrix <- as.list(bibble_factors)[[match.arg(matrix, names(bibble_factors))]]
  if (matrix == "uv") stop("No alignment method for both matrix factors.")
  prev_class <- setdiff(c(attr(x, "preclass"), class(x)), "bbl")[1]
  if (!(prev_class %in% c(unordered_classes, unsigned_classes))) {
    stop("Coordinates of class-'", prev_class, "' objects cannot be realigned.")
  }
  x <- to_bibble(x)
  M_x <- factor_uv(x, matrix)
  M_y <- factor_uv(y, matrix)
  # shared observations/variables
  x_df <- get_uv(x, matrix)
  y_df <- get_uv(y, matrix)
  if (is.null(by)) {
    M_x_i <- M_y_i <- 1:min(nrow(x_df), nrow(y_df))
  } else {
    M_x_j <- mutate(x_df, id = row_number())
    M_y_j <- mutate(y_df, id = row_number())
    M_x_i <- pull(left_join(y_df, M_x_j, by = by), id)
    M_y_i <- pull(left_join(x_df, M_y_j, by = by), id)
  }
  # reorder axes
  if (prev_class %in% unordered_classes) {
    r <- rank(x)
    for (i in 1:min(rank(x), rank(y))) {
      cosines <- t(M_x[M_x_i, i:r, drop = FALSE]) %*%
        M_y[M_y_i, i, drop = FALSE] /
        apply(M_x[M_x_i, i:r, drop = FALSE], 2, function(x) sum(x^2)) /
        sum(M_y[M_y_i, i] ^ 2)
      j <- which.min(abs(cosines)) + (i - 1)
      if (j != i) {
        x[["u"]][, c(i, j)] <- x[["u"]][, c(j, i)]
        x[["v"]][, c(i, j)] <- x[["v"]][, c(j, i)]
      }
    }
  }
  # reverse signs
  if (prev_class %in% unsigned_classes) {
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
    # signs of dot products of shared coordinates at shared obs/vars
    signs <- sign(apply((M_x[M_x_i, coords] * M_y[M_y_i, coords]), 2, sum))
    # reverse misaligned directions
    x[["u"]][, coords] <- sweep(x[["u"]][, coords], 2, signs, "/")
    x[["v"]][, coords] <- sweep(x[["v"]][, coords], 2, signs, "/")
  }
  x
}
