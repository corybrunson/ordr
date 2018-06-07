
as_bibble.lm <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}

get_u.lm <- function(x) {
  .intercept_col <- if (names(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  #cbind(.intercept_col, as.matrix(x$model[, -1]))
  cbind(.intercept_col, model.frame(x)[, -1])
}

get_v.lm <- function(x) {
  res <- t(x$coefficients)
  dimnames(res) <- list(names(x$model)[1], get_coord(x))
  res
}

get_coord.lm <- function(x) {
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

u_attr.lm <- function(x) {
  res <- tibble(.name = rownames(model.frame(x)))
  .int <- as.integer(names(x$coefficients)[1] == "(Intercept)")
  .rk <- x$rank
  bind_cols(
    res,
    select(broom::augment(x), -(1:(.rk - .int + 1)))
  )
}

v_attr.lm <- function(x) {
  tibble(
    .name = names(x$model)[1]
  )
}

coord_attr.lm <- function(x) {
  as_tibble(data.frame(
    .name = get_coord(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ))
}

get_u.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  cbind(.intercept_col, model.frame(x)[, -1])
}

get_v.mlm <- function(x) {
  res <- t(x$coefficients)
  colnames(res) <- get_coord(x)
  res
}

get_coord.mlm <- function(x) {
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

u_attr.mlm <- function(x) {
  tibble(
    .name = rownames(model.frame(x))
  )
}

v_attr.mlm <- function(x) {
  tibble(
    .name = colnames(x$coefficients)
  )
}

coord_attr.mlm <- function(x) {
  res <- as_tibble(data.frame(
    .name = get_coord(x),
    broom::tidy(x),
    stringsAsFactors = FALSE
  ))
  tidyr::nest(res, -.name, -term, .key = "model")
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
