#' @title **dplyr** verbs for `tbl_ord` factors
#'
#' @description These functions adapt [dplyr][dplyr::dplyr] verbs to the factors
#'   of a `tbl_ord`. The raw verbs are not defined for `tbl_ord`s; instead, each
#'   verb has two analogues, corresponding to the two matrix factors. They each
#'   rely on a common workhorse function, which takes the composition of the
#'   **dplyr** verb with `annotation_*`, applied to the factor, removes any
#'   variables corresponding to coordinates or already annotated, and only then
#'   assigns it as the new `"*_annotation"` attribute of `.data` (see
#'   [annotation]).
#'   

#' @name dplyr-verbs
#' @importFrom dplyr pull rename select mutate transmute bind_cols
#' @param .data A [tbl_ord].
#' @param var A variable specified as in [dplyr::pull()].
#' @param ... Comma-separated unquoted expressions as in, e.g.,
#'   [dplyr::select()].
#' @template param-matrix
#' @example inst/examples/iris-princomp-sec.r
#' @example inst/examples/benthos-ca-augment-confer.r

pull_factor <- function(.data, var = -1, .matrix) {
  pull(annotation_factor(.data, .matrix = .matrix), !! enquo(var))
}
#' @rdname dplyr-verbs
#' @export
pull_u <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "u")
}
#' @rdname dplyr-verbs
#' @export
pull_v <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "v")
}

rename_factor <- function(.data, ..., .matrix) {
  att <- rename(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
rename_u <- function(.data, ...) rename_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
rename_v <- function(.data, ...) rename_factor(.data, ..., .matrix = "v")

select_factor <- function(.data, ..., .matrix) {
  att <- select(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
select_u <- function(.data, ...) select_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
select_v <- function(.data, ...) select_factor(.data, ..., .matrix = "v")

mutate_factor <- function(.data, ..., .matrix) {
  att <- mutate(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
mutate_u <- function(.data, ...) mutate_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
mutate_v <- function(.data, ...) mutate_factor(.data, ..., .matrix = "v")

transmute_factor <- function(.data, ..., .matrix) {
  att <- transmute(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
transmute_u <- function(.data, ...) transmute_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
transmute_v <- function(.data, ...) transmute_factor(.data, ..., .matrix = "v")

bind_cols_factor <- function(.data, ..., .matrix) {
  att <- bind_cols(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
bind_cols_u <- function(.data, ...) bind_cols_factor(.data, ..., .matrix = "u")
#' @rdname dplyr-verbs
#' @export
bind_cols_v <- function(.data, ...) bind_cols_factor(.data, ..., .matrix = "v")
