#' Bibble functionality for linear model objects
#' 
#' These methods extract data from, and attribute new data to, objects of class
#' \code{"lm"}, \code{"glm"}, and \code{"mlm"}.
#' 
#' @name methods-lm
#' @template methods-params
#' @template matrix-param
#' @example inst/examples/ex-bibble-lm.r

#' @importFrom broom augment
#' @importFrom stats model.frame

#' @rdname methods-lm
#' @export
as_bibble.lm <- as_bibble_recognized

#' @rdname methods-lm
#' @export
recover_u.lm <- function(x) {
  .intercept_col <- if (names(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  .predictors <- as.matrix(model.frame(x)[, -1])
  res <- cbind(.intercept_col, .predictors)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_v.lm <- function(x) {
  res <- t(x$coefficients)
  dimnames(res) <- list(
    if (is.matrix(x$model[, 1])) colnames(x$model[, 1]) else names(x$model)[1],
    recover_coord(x)
  )
  res
}

#' @rdname methods-lm
#' @export
recover_coord.lm <- function(x) {
  .predictors <- x$model[, -1]
  if (is.matrix(.predictors)) {
    coord <- colnames(.predictors)
  } else {
    coord <- names(.predictors)
    mat_coord <- which(sapply(.predictors, is.matrix))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(.predictors[, i])
    )))
  }
  if (names(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}

#' @rdname methods-lm
#' @export
augment_u.lm <- function(x) {
  res <- tibble(.name = rownames(model.frame(x)))
  .int <- as.integer(names(x$coefficients)[1] == "(Intercept)")
  .rk <- x$rank
  dplyr::bind_cols(
    res,
    dplyr::select(augment(x), -(1:(.rk - .int + 1)))
  )
}

#' @rdname methods-lm
#' @export
augment_v.lm <- function(x) {
  tibble(
    .name = if (is.matrix(x$model[, 1])) {
      colnames(x$model[, 1])
    } else {
      names(x$model)[1]
    }
  )
}

#' @rdname methods-lm
#' @export
augment_coord.lm <- function(x) {
  as_tibble(data.frame(
    .name = recover_coord(x),
    tidy(un_bibble(x)),
    stringsAsFactors = FALSE
  ))
}

#' @rdname methods-lm
#' @export
permute_to.lm <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get permutations
  p <- permutation_to(get_factor(as_bibble(x), .matrix), y)
  # tag 'cmds' object with permutation
  x <- attribute_alignment(x, diag(1, nrow = x$rank)[, p, drop = FALSE])
  # return annotated object
  x
}

#' @rdname methods-lm
#' @export
recover_u.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  .predictors <- as.matrix(model.frame(x)[, -1])
  res <- cbind(.intercept_col, .predictors)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_v.mlm <- function(x) {
  res <- t(x$coefficients)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_coord.mlm <- function(x) {
  .predictors <- x$model[, -1]
  if (is.matrix(.predictors)) {
    coord <- colnames(.predictors)
  } else {
    coord <- names(.predictors)
    mat_coord <- which(sapply(.predictors, is.matrix))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(.predictors[, i])
    )))
  }
  if (rownames(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}

#' @rdname methods-lm
#' @export
augment_u.mlm <- function(x) {
  tibble(
    .name = rownames(model.frame(x))
  )
}

#' @rdname methods-lm
#' @export
augment_v.mlm <- function(x) {
  tibble(
    .name = colnames(x$coefficients)
  )
}

#' @rdname methods-lm
#' @export
augment_coord.mlm <- function(x) {
  res <- as_tibble(data.frame(
    .name = recover_coord(x),
    tidy(un_bibble(x)),
    stringsAsFactors = FALSE
  ))
  tidyr::nest(res, -dplyr::one_of(".name", "term"), .key = "model")
}

#' @rdname methods-lm
#' @export
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
