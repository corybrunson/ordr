library(magrittr)
readxl::read_xlsx(here::here("data-raw/glass.xlsx")) %>%
  tidyr::fill(Site) %>%
  dplyr::mutate_at(5:16, as.numeric) ->
  glass
usethis::use_data(glass)
