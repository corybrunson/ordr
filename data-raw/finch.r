finches <- read.csv(here::here("data-raw", "finch.txt"), row.names = 1)
devtools::use_data(finches)
