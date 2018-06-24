#' *dplyr* verbs
#' 
#' These functions adapt \strong{\link[dplyr]{dplyr}} verbs to the factors of a 
#' \code{tbl_ord}. The raw verbs are not defined for \code{tbl_ord}s; instead, 
#' each verb has two analogues, corresponding to the two matrix factors. They 
#' each rely on a common workhorse function, which takes the composition of the 
#' \strong{dplyr} verb with \code{\link{fortify}}, applied to the factor, 
#' removes any variables corresponding to coordinates or produced by 
#' \code{augment_*} (see \code{\link{augmentation}}), and only then assigns it
#' as the new \code{"*_annot"} attribute of \code{.data}.
#' 

#' @name dplyr-verbs
#' @importFrom dplyr pull rename select mutate transmute bind_cols
#' @param .data A \code{\link{tbl_ord}}.
#' @param var A variable specified as in \code{\link[dplyr]{pull}}.
#' @param ... Comma-separated unquoted expressions as in, e.g.,
#'   \code{\link[dplyr]{select}}.
#' @template matrix-param

pull_factor <- function(.data, var = -1, .matrix) {
  pull(fortify(.data, .matrix = .matrix), !!enquo(var))
}
#' @rdname dplyr-verbs
#' @export
pull_u <- function(.data, var = -1) pull_factor(.data, var, .matrix = "u")
#' @rdname dplyr-verbs
#' @export
pull_v <- function(.data, var = -1) pull_factor(.data, var, .matrix = "v")

rename_factor <- function(.data, ..., .matrix) {
  att <- rename(fortify(.data, .matrix = .matrix), ...)
  set_factor_annot(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
rename_u <- function(.data, ...) rename_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
rename_v <- function(.data, ...) rename_factor(.data, ..., .matrix = "v")

select_factor <- function(.data, ..., .matrix) {
  att <- select(fortify(.data, .matrix = .matrix), ...)
  set_factor_annot(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
select_u <- function(.data, ...) select_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
select_v <- function(.data, ...) select_factor(.data, ..., .matrix = "v")

mutate_factor <- function(.data, ..., .matrix) {
  att <- mutate(fortify(.data, .matrix = .matrix), ...)
  set_factor_annot(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
mutate_u <- function(.data, ...) mutate_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
mutate_v <- function(.data, ...) mutate_factor(.data, ..., .matrix = "v")

transmute_factor <- function(.data, ..., .matrix) {
  att <- transmute(fortify(.data, .matrix = .matrix), ...)
  set_factor_annot(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
transmute_u <- function(.data, ...) transmute_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
transmute_v <- function(.data, ...) transmute_factor(.data, ..., .matrix = "v")

bind_cols_factor <- function(.data, ..., .matrix) {
  att <- bind_cols(fortify(.data, .matrix = .matrix), ...)
  set_factor_annot(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
bind_cols_u <- function(.data, ...) bind_cols_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
bind_cols_v <- function(.data, ...) bind_cols_factor(.data, ..., .matrix = "v")
