as_bibble.lm <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_u.lm <- function(x) {
  .intercept_col <- if (names(x$coefficients)[1] == "(Intercept)") {
    tibble(`(Intercept)` = rep(1L, nrow(x$model)))
  } else NULL
  .model_rk <- x$rank
  res <- bind_cols(
    .intercept_col,
    x$model[, -1] %>%
      as.data.frame() %>% lapply(as.vector) %>% as_tibble()
  ) %>%
    setNames(clean_coordinates(x)) %>%
    mutate(name = rownames(model.frame(x))) %>%
    bind_cols(
      x %>%
        broom::augment() %>%
        select(-(1:(.model_rk - length(.intercept_col) + 1)))
    )
}
get_v.lm <- function(x) {
  x$coefficients %>%
    t() %>%
    as_tibble() %>%
    setNames(clean_coordinates(x))
}
get_coordinates.lm <- function(x) {
  data.frame(
    .name = clean_coordinates(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble()
}
clean_coordinates.lm <- function(x) {
  if (is.matrix(x$model[, -1])) {
    coord <- colnames(x$model[, -1])
  } else {
    coord <- names(x$model[, -1])
    mat_coord <- which(sapply(x$model[, -1], is.matrix))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(x$model[, -1][, i])
    )))
  }
  if (names(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}
get_data.lm <- function(x) {
  res <- as_tibble(m$model)
  if (names(x$coefficients)[1] == "(Intercept)") {
    res$`(Intercept)` <- 1
  }
  res
}

get_u.glm <- get_u.lm
get_v.glm <- get_v.lm
get_coordinates.glm <- get_coordinates.lm
clean_coordinates.glm <- clean_coordinates.lm

get_u.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    tibble(`(Intercept)` = rep(1L, nrow(x$model)))
  } else NULL
  bind_cols(
    .intercept_col,
    model.frame(x)[, -1] %>%
      as.data.frame() %>% lapply(as.vector) %>% as_tibble()
  ) %>%
    setNames(clean_coordinates(x)) %>%
    mutate(name = rownames(model.frame(x)))
}
get_v.mlm <- function(x) {
  x$coefficients %>%
    t() %>%
    as_tibble() %>%
    setNames(clean_coordinates(x)) %>%
    mutate(name = colnames(x$coefficients))
}
get_coordinates.mlm <- function(x) {
  data.frame(
    .name = clean_coordinates(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    tidyr::nest(-.name, -term, .key = "model")
}
clean_coordinates.mlm <- function(x) {
  if (is.matrix(x$model[, -1])) {
    coord <- colnames(x$model[, -1])
  } else {
    coord <- names(x$model[, -1])
    mat_coord <- which(sapply(x$model[, -1], is.matrix))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(x$model[, -1][, i])
    )))
  }
  if (rownames(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}
