# Log-ratio analysis of Ruhlman's recipe ratios
ratios %>%
  dplyr::select(recipe, flour, egg, butter) %>%
  dplyr::filter(flour > 0 & egg > 0 & butter > 0) %>%
  dplyr::distinct(flour, egg, butter) ->
  sub_ratios
