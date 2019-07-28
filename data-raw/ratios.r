library(tidyverse)
ratio_sheets <- readxl::excel_sheets(here::here("data-raw/ratios.xlsx"))
map(ratio_sheets, readxl::read_xlsx,
    path = here::here("data-raw/ratios.xlsx"), col_names = TRUE) %>%
  map2(ratio_sheets, ~ mutate(.x, chapter = .y)) %>%
  bind_rows() %>%
  select(chapter, recipe, everything()) %>%
  mutate_all(replace_na, replace = 0) ->
  ratios
usethis::use_data(ratios)
