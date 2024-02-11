
ggbipairs <- function(
    ordination, coords = NULL,
) {
  
  # all ordination dimensions
  dims <- recover_coord(data)
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
      cols = dims,
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
  ord_pairs
}
