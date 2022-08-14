#' @title Fit an ordination model to a data object
#'
#' @description This is a convenience function to fit an ordination model to a
#'   specified subset of columns of a data frame and augment the tbl_ord object
#'   with both its intrinsic diagnostics (via `[augment_ord()]`) and any
#'   additional columns of the data frame (via `[cbind_rows()]`).
#'   

#' @details
#' 
#' (Write this up.)

#' @name ordinate
#' @importFrom rlang expr enexpr enexprs enquo set_names
#' @importFrom rlang  is_formula is_function as_function is_empty
#' @importFrom tidyselect eval_select
#' @param x A data object to be passed to the `model`, such as an [array],
#'   [table], [data.frame], or [stats::dist].
#' @param model An ordination function whose output is coercible to class
#'   '[tbl_ord]', or a symbol or character string (handled by [match.fun()]).
#'   Alternatively, a formula `~ fun(., ...)` where `fun` is such a function and
#'   other arguments are explicit, which will be evaluated with `x` in place of
#'   `.`.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> If `x` is a data
#'   frame, columns to pass to `model`. If missing, all columns are used.
#' @param augment <[`tidy-select`][tidyr_tidy_select]> If `x` is a data frame,
#'   columns to augment to the row data of the ordination. If missing, all
#'   columns not included in `cols` will be augmented.
#' @param ... Additional arguments passed to `model`.
#' @return An augmented tbl_ord.
#' @example inst/examples/ex-ordinate.r
#' @export
ordinate <- function(x, model, cols, augment, ...) {
  UseMethod("ordinate")
}

#' @rdname ordinate
#' @export
ordinate.default <- function(x, model, ...) {
  model <- ensure_model(model)
  
  # fit the model
  ord <- model(x, ...)
  
  # coerce to class 'tbl_ord'
  ord <- as_tbl_ord(ord)
  
  # augment ordination with model specs
  ord <- augment_ord(ord)
  
  ord
}

#' @rdname ordinate
#' @export
ordinate.array <- ordinate.default

#' @rdname ordinate
#' @export
ordinate.table <- ordinate.default

#' @rdname ordinate
#' @export
#' @method ordinate data.frame
ordinate.data.frame <- function(x, model, cols, augment, ...) {
  
  # preserve any column-level attributes
  attrs <- attributes(x)
  is_col_attr <- function(y) {
    length(y) == ncol(x) &&
      ! is.null(names(y)) && all(names(y) == names(x)) &&
      # not just the column names; can get these using `augment_ord()`
      ! all(y == colnames(x))
  }
  x_attr <- as.data.frame(attrs[vapply(attrs, is_col_attr, FALSE)])
  
  # select ordination columns
  if (missing(cols)) cols <- names(x)
  cols_pos <- eval_select(enquo(cols), data = x)
  x_ord <- set_names(x[cols_pos], names(cols_pos))
  # select augmentation columns
  if (missing(augment)) {
    augment <- setdiff(seq_along(x), unname(cols_pos))
  }
  aug_pos <- eval_select(enquo(augment), data = x)
  x_aug <- set_names(x[aug_pos], names(aug_pos))
  
  # run the default procedure
  ord <- ordinate.default(x_ord, model, ...)
  
  # augment column attributes
  if (! is_empty(x_attr)) {
    x_attr <- x_attr[cols_pos, , drop = FALSE]
    ord <- cbind_cols(ord, x_attr)
  }
  
  # bind augmentation columns to row data
  if (! is_empty(x_aug)) {
    ord <- if (nrow(x_aug) == nrow(ord)) {
      cbind_rows(ord, x_aug)
    } else if (nrow(x_aug) == nrow(get_rows(ord, elements = "active"))) {
      cbind_rows(ord, x_aug, elements = "active")
    } else if (nrow(x_aug) == nrow(get_rows(ord, elements = "score"))) {
      cbind_rows(ord, x_aug, elements = "score")
    } else ord
  }
  
  ord
}

#' @rdname ordinate
#' @export
ordinate.dist <- ordinate.default

# pre-process the model argument
# adapted from `purrr::as_mapper()`
ensure_model <- function(model) {
  if (is_formula(model)) model <- as_function(model)
  if (is.symbol(model) || is.character(model)) model <- match.fun(model)
  model
}
