
as_bibble.lm <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_u.lm <- function(x) {
  .intercept_col <- if (names(x$coefficients)[1] == "(Intercept)") {
    tibble(`(Intercept)` = rep(1L, nrow(x$model)))
  } else NULL
  .model_rk <- x$rank
  res <- rlang::set_names(bind_cols(
    .intercept_col,
    as_tibble(lapply(as.data.frame(x$model[, -1]), as.vector))
  ), clean_coordinates(x))
  res$name <- rownames(model.frame(x))
  bind_cols(
    res,
    select(broom::augment(x), -(1:(.model_rk - length(.intercept_col) + 1)))
  )
}
get_v.lm <- function(x) {
  rlang::set_names(as_tibble(t(x$coefficients)), clean_coordinates(x))
}
get_coordinates.lm <- function(x) {
  as_tibble(data.frame(
    .name = clean_coordinates(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ))
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

get_u.glm <- get_u.lm
get_v.glm <- get_v.lm
get_coordinates.glm <- get_coordinates.lm
clean_coordinates.glm <- clean_coordinates.lm

get_u.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    tibble(`(Intercept)` = rep(1L, nrow(x$model)))
  } else NULL
  res <- rlang::set_names(bind_cols(
    .intercept_col,
    as_tibble(lapply(as.data.frame(model.frame(x)[, -1]), as.vector))
  ), clean_coordinates(x))
  res$name <- rownames(model.frame(x))
  res
}
get_v.mlm <- function(x) {
  res <- rlang::set_names(as_tibble(t(x$coefficients)), clean_coordinates(x))
  res$name <- colnames(x$coefficients)
  res
}
get_coordinates.mlm <- function(x) {
  res <- as_tibble(data.frame(
    .name = clean_coordinates(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ))
  tidyr::nest(res, -.name, -term, .key = "model")
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

reconstruct.lm <- function(x) {
  pred_mat <- as.matrix(x$model[, -1, drop = FALSE])
  names_fun <- if (class(x)[1] == "lm") names else rownames
  if (names_fun(x$coefficients)[1] == "(Intercept)") {
    pred_mat <- cbind(`(Intercept)` = 1, pred_mat)
  }
  coef_mat <- as.matrix(x$coefficients)
  if (class(x)[1] != "mlm") colnames(coef_mat) <- names(x$model)[1]
  as.data.frame(pred_mat %*% coef_mat)
}
