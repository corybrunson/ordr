finches <- here::here("data-raw", "finch.txt") %>%
  read.csv(row.names = 1) %>%
  t() %>% as_tibble()
devtools::use_data(finches)
