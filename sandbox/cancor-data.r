library(tidyverse)

get_keyword_data <- function(pkgs, detect = ".") {
  tibble(package = pkgs) |>
    mutate(data = map(package, \(pkg) as_tibble(data(package = pkg)$results))) |>
    unnest(data) |>
    transmute(package, name = Item, title = Title) |>
    # `?help`
    mutate(help = map2(name, package, \(x, y) help((x), (y)))) |>
    mutate(doc = map(help, \(h) try(utils:::.getHelpFile(h)))) |>
    filter(! map_lgl(doc, \(d) inherits(d, "try-error"))) |>
    mutate(doc = map(doc, \(rd) str_c(as.character(rd), collapse = "\n"))) |>
    # pull(doc) |> first() -> test
    # detect keyword
    filter(str_detect(doc, detect))
}
get_data <- function(x, package) {
  env <- new.env()
  name <- do.call(data, list(x, package = package, envir = env))
  get(name, envir = env)
}

get_keyword_data(
  c("datasets", "stats", "MASS"),
  detect = "data[\\. ]{1}frame|tibble"
) |> 
  mutate(data = map2(package, name, \(x, y) get_data(y, x))) |> 
  # data frames only
  filter(map_lgl(data, is.data.frame)) |> 
  # relatively large number of cases
  filter(map_int(data, \(d) nrow(d) %||% 0L) > 36L) |> 
  # moderate number of numeric variables
  filter(map_int(data, \(d) ncol(select(d, where(is.numeric))) %||% 0L) > 8L) |> 
  print() -> df_data

?datasets::USJudgeRatings
?MASS::Boston
?MASS::UScereal
?MASS::UScrime
?MASS::biopsy
?MASS::fgl
?MASS::gilgais

head(MASS::Boston)
# environmental: crim, indus, chas, nox, dis, rad, tax, ptratio, black, lstat
# housing: zn, rm, age, tax, medv
boston_cca <- 
  ordinate(
    x = subset(MASS::Boston, select = c(crim, indus, chas, nox, dis, rad, tax, ptratio, black, lstat)),
    y = subset(MASS::Boston, select = c(zn, rm, age, tax, medv)),
    cancor_ord, xcenter = FALSE, ycenter = FALSE, scores = TRUE
  )
ggbiplot(boston_cca) +
  geom_vector(stat = rows_stat("identity")) +
  geom_point(stat = cols_stat("identity"))

head(MASS::biopsy)
# aggregate behaviors: V1, V4, V7?
# averaged structures: V2, V3, V5, V6, V8, V9

head(MASS::gilgais)
gilgais_cca <- 
  ordinate(x = MASS::gilgais[, 1:3], y = MASS::gilgais[, 4:6], cancor_ord)
tidy(gilgais_cca)
ggbiplot(gilgais_cca, sec.axes = "cols") +
  coord_scaffold() + theme_scaffold() +
  geom_vector(stat = rows_stat("identity")) +
  geom_text(
    stat = rows_stat("identity"), aes(label = name),
    hjust = "outward", vjust = "outward"
  ) +
  geom_vector(stat = cols_stat("identity"), arrow = NULL) +
  geom_point(stat = cols_stat("identity")) +
  geom_text(
    stat = cols_stat("identity"), aes(label = name),
    hjust = "outward", vjust = "outward"
  )
?candisc:::Wilks.cancor
candisc:::Wilks.cancor

# test
library(ordr.extra)
gilgais_cca <- 
  ordinate(x = MASS::gilgais[, 1:3], y = MASS::gilgais[, 4:6], candisc::cancor)
candisc:::Wilks.cancor(gilgais_cca)
