#' @title Specify columns of a data frame to ordinate and augment
#' 
#' @description 

#' @name ordinate
#' @importFrom rlang expr enexpr enexprs enquo set_names is_formula is_function
#' @importFrom tidyselect eval_select
#' @param data A data frame.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> Columns of `data` to
#'   pass to `model`.
#' @param model An ordination function whose output is coercible to class
#'   '[tbl_ord]'.
#' @param augment <[`tidy-select`][tidyr_tidy_select]> Columns of `data` to
#'   augment to the row data of the ordination.
#' @param ... Additional arguments passed to `model`.
#' @example inst/examples/ex-ordinate.r
#' @export
ordinate <- function(
  data, cols, model, augment = NULL, ...
) {
  # select ordination columns
  cols_pos <- eval_select(enquo(cols), data = data)
  data_ord <- set_names(data[cols_pos], names(cols_pos))
  # select augmentation columns
  if (is.null(enexpr(augment)))
    augment <- setdiff(seq_along(data), unname(cols_pos))
  aug_pos <- eval_select(enquo(augment), data = data)
  data_aug <- set_names(data[aug_pos], names(aug_pos))
  
  # fit the ordination model
  if (is_formula(model)) {
    stop("Not yet implemented.")
  } else if (is_function(model)) {
    ord <- model(data_ord, ...)
  } else {
    stop("Unrecognized model type; pass a formula or a function.")
  }
  
  # coerce to class 'tbl_ord'
  ord <- as_tbl_ord(ord)
  # augment ordination with model specs
  ord <- augment_ord(ord)
  # bind augmentation columns to row data
  ord <- bind_cols_rows(ord, data_aug)
  
  ord
}
