#' @title Fit an ordination model to columns of a data frame
#'
#' @description This is a convenience function to fit an ordination model to a
#'   specified subset of columns of a data frame and augment the tbl_ord object
#'   with both its intrinsic diagnostics (via `[augment_ord()]`) and any
#'   additional columns of the data frame (via `[bind_cols_rows()]`).
#'   

#' @name ordinate
#' @importFrom rlang expr enexpr enexprs enquo set_names
#' @importFrom rlang  is_formula is_function as_function is_empty
#' @importFrom tidyselect eval_select
#' @param data A data frame.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> Columns of `data` to
#'   pass to `model`.
#' @param model An ordination function whose output is coercible to class
#'   '[tbl_ord]'. Alternatively, a formula `~ fun(., ...)` where `fun` is such a
#'   function and other arguments are explicit, which will be evaluated with
#'   `data` in place of `.`.
#' @param augment <[`tidy-select`][tidyr_tidy_select]> Columns of `data` to
#'   augment to the row data of the ordination. If missing, all columns not
#'   included in `cols` will be augmented.
#' @param ... Additional arguments passed to `model`.
#' @example inst/examples/ex-ordinate.r
#' @export
ordinate <- function(
  data, cols, model, augment, ...
) {
  # preserve any column-level attributes
  attrs <- attributes(data)
  is_col_attr <- function(x) {
    length(x) == ncol(data) &&
      ! is.null(names(x)) && all(names(x) == names(data)) &&
      # not just the column names; can get these using `augment_ord()`
      ! all(x == colnames(data))
  }
  data_attr <- as.data.frame(attrs[vapply(attrs, is_col_attr, FALSE)])
  # convert matrix to data frame
  if (is.matrix(data)) data <- as.data.frame(data)
  
  # select ordination columns
  cols_pos <- eval_select(enquo(cols), data = data)
  data_ord <- set_names(data[cols_pos], names(cols_pos))
  # select augmentation columns
  if (missing(augment)) {
    augment <- setdiff(seq_along(data), unname(cols_pos))
  }
  aug_pos <- eval_select(enquo(augment), data = data)
  data_aug <- set_names(data[aug_pos], names(aug_pos))
  
  # fit the ordination model
  # adapted from `purrr::as_mapper()`
  if (is_formula(model)) model <- as_function(model)
  ord <- model(data_ord, ...)
  
  # coerce to class 'tbl_ord'
  ord <- as_tbl_ord(ord)
  # augment column attributes
  if (! is_empty(data_attr)) {
    data_attr <- data_attr[cols_pos, , drop = FALSE]
    ord <- bind_cols_cols(ord, data_attr)
  }
  # augment ordination with model specs
  ord <- augment_ord(ord)
  # bind augmentation columns to row data
  if (! is_empty(data_aug)) ord <- bind_cols_rows(ord, data_aug)
  
  ord
}
