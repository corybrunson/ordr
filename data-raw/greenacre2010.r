
here::here("data-raw/greenacre2010-ex2-1.txt") %>%
  readr::read_delim(delim = " ", col_names = FALSE) %>%
  setNames(c(letters[1:5], "Pollution", "Depth", "Temperature", "Sediment")) %>%
  dplyr::mutate(site = paste0("s", 1:30)) %>%
  dplyr::select(site, dplyr::everything()) ->
  bioenv
devtools::use_data(bioenv)

country_differences <- read.delim(
  here::here("data-raw/greenacre2010-ex4-1.txt"),
  header = TRUE, sep = " "
)
rownames(country_differences) <- country_differences$Countries
country_differences <- as.matrix(country_differences[, -1])
devtools::use_data(country_differences)

country_attributes <- read.delim(
  here::here("data-raw/greenacre2010-ex4-3.txt"),
  header = TRUE, sep = " "
)
rownames(country_attributes) <- country_attributes$Countries
country_attributes <- as.matrix(country_attributes[, -1])
devtools::use_data(country_attributes)

# 'benthos' data set
# https://www.fbbva.es/microsite/multivariate-statistics/data.html
benthos <- as.data.frame(readxl::read_xls(here::here("data-raw/benthos.xls")))
rownames(benthos) <- unlist(benthos[, 1])
benthos <- as.matrix(benthos[, -1])
devtools::use_data(benthos)

# 'issp_women' data set
# https://www.fbbva.es/microsite/multivariate-statistics/data.html
issp_women <- readxl::read_xls(here::here("data-raw/women_Spain2002_concat.xls")) %>%
  as.data.frame()
rownames(issp_women) <- unlist(issp_women[, 1])
issp_women <- as.matrix(issp_women[, -1])
mode(issp_women) <- "integer"
devtools::use_data(issp_women)
