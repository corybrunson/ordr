#' @title **dplyr** verbs for tbl_ord factors
#'
#' @description These functions adapt [dplyr][dplyr::dplyr] verbs to the factors
#'   of a [tbl_ord]. The raw verbs are not defined for tbl_ords; instead, each
#'   verb has two analogues, corresponding to the two matrix factors. They each
#'   rely on a common workhorse function, which takes the composition of the
#'   **dplyr** verb with `annotation_*`, applied to the factor, removes any
#'   variables corresponding to coordinates or already annotated, and only then
#'   assigns it as the new `"*_annotation"` attribute of `.data` (see
#'   [annotation]).
#'   

#' @name dplyr-verbs
#' @importFrom tidyselect one_of
#' @importFrom dplyr pull rename select mutate transmute bind_cols left_join
#' @param .data An object of class '[tbl_ord]'.
#' @param var A variable specified as in [dplyr::pull()].
#' @param ... Comma-separated unquoted expressions as in, e.g.,
#'   [dplyr::select()].
#' @template param-matrix

pull_factor <- function(.data, var = -1, .matrix) {
  pull(annotation_factor(.data, .matrix = .matrix), !! enquo(var))
}
#' @rdname dplyr-verbs
#' @export
pull_rows <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
pull_cols <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "cols")
}

rename_factor <- function(.data, ..., .matrix) {
  att <- rename(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
rename_rows <- function(.data, ...) {
  rename_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
rename_cols <- function(.data, ...) {
  rename_factor(.data, ..., .matrix = "cols")
}

select_factor <- function(.data, ..., .matrix) {
  att <- select(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
select_rows <- function(.data, ...) {
  select_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
select_cols <- function(.data, ...) {
  select_factor(.data, ..., .matrix = "cols")
}

mutate_factor <- function(.data, ..., .matrix) {
  crd <- as_tibble(get_factor(.data, .matrix = .matrix))
  att <- annotation_factor(.data, .matrix = .matrix)
  tbl <- bind_cols(crd, att)
  tbl <- mutate(tbl, ...)
  tbl <- select(tbl, -one_of(intersect(names(crd), names(tbl))))
  set_annotation_factor(.data, tbl, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
mutate_rows <- function(.data, ...) {
  mutate_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
mutate_cols <- function(.data, ...) {
  mutate_factor(.data, ..., .matrix = "cols")
}

transmute_factor <- function(.data, ..., .matrix) {
  crd <- as_tibble(get_factor(.data, .matrix = .matrix))
  att <- annotation_factor(.data, .matrix = .matrix)
  tbl <- bind_cols(crd, att)
  tbl <- transmute(tbl, ...)
  tbl <- select(tbl, -one_of(intersect(names(crd), names(tbl))))
  set_annotation_factor(.data, tbl, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
transmute_rows <- function(.data, ...) {
  transmute_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
transmute_cols <- function(.data, ...) {
  transmute_factor(.data, ..., .matrix = "cols")
}

cbind_factor <- function(.data, ..., .matrix) {
  ann_fac <- annotation_factor(.data, .matrix = .matrix)
  att <- if (nrow(ann_fac) == 0L) tibble(...) else bind_cols(ann_fac, ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
cbind_rows <- function(.data, ...) {
  cbind_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
cbind_cols <- function(.data, ...) {
  cbind_factor(.data, ..., .matrix = "cols")
}

left_join_factor <- function(.data, ..., .matrix) {
  att <- left_join(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
left_join_rows <- function(.data, ...) {
  left_join_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
left_join_cols <- function(.data, ...) {
  left_join_factor(.data, ..., .matrix = "cols")
}
