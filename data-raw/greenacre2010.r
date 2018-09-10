
simple_example <- make_bibble(
  u = cbind(x = c(2, 1, -1, 1, 2), y = c(2, 2, 1, -1, -2)),
  v = cbind(x = c(3, 2, -1, -2), y = c(1, -1, 2, -1))
)
devtools::use_data(simple_example)

bioenv <- here::here("data-raw/greenacre2010-ex2-1.txt") %>%
  readr::read_delim(delim = " ", col_names = FALSE) %>%
  setNames(c(letters[1:5], "Pollution", "Depth", "Temperature", "Sediment")) %>%
  dplyr::mutate(site = paste0("s", 1:30)) %>%
  dplyr::select(site, 1:9)
devtools::use_data(bioenv)

country_differences <- here::here("data-raw/greenacre2010-ex4-1.txt") %>%
  readr::read_delim(delim = " ", col_names = TRUE)
devtools::use_data(country_differences)
country_attributes <- here::here("data-raw/greenacre2010-ex4-3.txt") %>%
  readr::read_delim(delim = " ", col_names = TRUE)
devtools::use_data(country_attributes)
