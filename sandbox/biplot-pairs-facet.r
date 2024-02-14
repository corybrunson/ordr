
ggbipairs <- function(
    ordination, coords = NULL
) {
  
  # all ordination dimensions
  dims <- recover_coord(ordination)
  # restrict to explicitly specified dimensions, if any
  if (! is.null(coords)) {
    if (is.numeric(coords)) {
      dims <- dims[intersect(seq_along(dims), coords)]
    } else if (is.character(coords)) {
      dims <- dims[match(coords, dims)]
    } else {
      warning("`coords` is neither numeric nor character and will be ignored.")
    }
  }
  
  # pivot dimensions
  ordination |> 
    fortify(.matrix = "both") |> 
    # NOTE: Ensure that '.id' is not already a column name.
    dplyr::mutate(.id = dplyr::row_number()) |> 
    tidyr::pivot_longer(
      cols = tidyselect::all_of(dims),
      # NOTE: Ensure that '.axis' and '.value' are not already column names.
      names_to = ".axis", values_to = ".value"
    ) |>
    print() -> ord_long
  
  # self-join all dimensions by row/col `.id`
  ord_long |> 
    # NOTE: Creates all pairings, with 1D biplots along diagonal
    dplyr::full_join(
      ord_long, by = setdiff(names(ord_long), c(".axis", ".value")),
      relationship = "many-to-many", suffix = c("_x", "_y")
    ) |> 
    ggbiplot(aes(x = .value_x, y = .value_y)) +
    # NOTE: `coord_fixed()` doesn't support free scales.
    facet_grid(rows = vars(.axis_y), cols = vars(.axis_x)) ->
    ord_pairs
  
  # return faceted plot that admits additional layers
  # FIXME: Need to prevent addition of another facet layer.
  ord_pairs + labs(x = NULL, y = NULL)
}

ggbipairs(iris_pca, coords = seq(3)) +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_ellipse(aes(color = Species), alpha = .5, level = .99) +
  geom_cols_vector() +
  geom_cols_text_radiate(aes(label = name)) +
  expand_limits(y = c(-3.5, NA)) +
  ggtitle("PCA of Anderson's iris measurements",
          "99% confidence ellipses; variables use top & right axes")
