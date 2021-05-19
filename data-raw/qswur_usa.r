library(magrittr)
library(readxl)
library(dplyr)

# download all spreadsheets into 'ignore' folder

names_qs <- c(
  "institution", "country", "size", "focus", "res", "age", "status",
  "rk_academic", "rk_employer",
  "rk_ratio", "rk_citations",
  "rk_intl_faculty", "rk_intl_students"
)

read_qs1 <- function(path, year) {
  path %>%
    read_xlsx(sheet = 1, skip = 3) %>%
    select(7, 8, 9:13, seq(15, 25, 2)) %>%
    set_names(names_qs) %>%
    mutate(year = year)
}

read_qs1("ignore/QS World University Rankings 2016-2017.xlsx", 2017L)

read_qs2 <- function(path, year) {
  path %>%
    read_xlsx(sheet = 1, skip = 4) %>%
    select(3, 4, 5:9, seq(11, 21, 2)) %>%
    set_names(names_qs) %>%
    mutate(year = year)
}

read_qs2("ignore/2018-QS-World-University-Rankings-v1.1.1.xlsx", 2018L)
read_qs2("ignore/2019-QS-World-University-Rankings-v1.0.xlsx", 2019L)
read_qs2("ignore/2020-QS-World-University-Rankings-v1.0.xlsx", 2020L)

qswur <- bind_rows(
  read_qs1("ignore/QS World University Rankings 2016-2017.xlsx", 2017L),
  read_qs2("ignore/2018-QS-World-University-Rankings-v1.1.1.xlsx", 2018L),
  read_qs2("ignore/2019-QS-World-University-Rankings-v1.0.xlsx", 2019L),
  read_qs2("ignore/2020-QS-World-University-Rankings-v1.0.xlsx", 2020L)
)
qswur %>%
  filter(country == "US" | country == "United States") %>%
  count(year)

qswur %>%
  filter(country == "US" | country == "United States") %>%
  select(-country) %>%
  mutate_at(vars(starts_with("rk_")), as.integer) %>%
  mutate(
    size = factor(size, c("S", "M", "L", "XL")),
    focus = factor(focus, c("SP", "FO", "CO", "FC")),
    age = as.integer(age),
    res = factor(res, c("LO", "MD", "HI", "VH"))
  ) %>%
  mutate(institution = toupper(institution)) %>%
  add_count(institution, name = "n") %>%
  filter(n == max(n)) %>%
  select(year, everything(), -n) ->
  qswur_usa

use_data(qswur_usa)
