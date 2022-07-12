# data frame of violent crime arrests in the United States
class(USArrests)
head(USArrests)
# get state abbreviation data
state <- data.frame(
  .name = state.name,
  .abb = state.abb
)
# compute (non-compositional, unweighted) log-ratio analysis
USArrests %>%
  subset(select = -UrbanPop) %>%
  lra() %>%
  as_tbl_ord() %>%
  print() -> arrests_lra
# augment log-ratio profiles with names and join state abbreviations
arrests_lra %>%
  augment_ord() %>%
  left_join_rows(state, by = ".name") %>%
  print() -> arrests_lra
# recover state and arrest profiles
head(get_rows(arrests_lra))
get_cols(arrests_lra)
# initially, inertia is conferred on neither factor
get_conference(arrests_lra)
