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
#' @importFrom dplyr pull rename select mutate transmute bind_rows bind_cols
#'   left_join
#' @param .data An object of class '[tbl_ord]'.
#' @param var A variable specified as in [dplyr::pull()].
#' @param ... Comma-separated unquoted expressions as in, e.g.,
#'   [dplyr::select()].
#' @param elements Character; which elements of each factor to which to bind new
#'   annotation data. One of `"all"` (the default), `"active"`, or
#'   `"supplementary"`, with partial matching.
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

cbind_factor <- function(.data, ..., .matrix, elements = "all") {
  ann_fac <- annotation_factor(.data, .matrix = .matrix)
  att_fac <- if (elements == "all" || ! ".supplement" %in% names(.data)) {
    tibble(...)
  } else if (elements == "supplementary") {
    n_p <- nrow(.data[.data$.matrix == .matrix & .data$.supplement == FALSE,
                      , drop = FALSE])
    bind_rows(tibble_pole(nrow = n_p), ...)
  } else if (elements == "active") {
    n_s <- nrow(.data[.data$.matrix == .matrix & .data$.supplement == TRUE,
                      , drop = FALSE])
    bind_rows(..., tibble_pole(nrow = n_s))
  }
  att <- if (nrow(ann_fac) == 0L) att_fac else bind_cols(ann_fac, att_fac)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
cbind_rows <- function(.data, ..., elements = "all") {
  cbind_factor(.data, ..., .matrix = "rows", elements = "all")
}
#' @rdname dplyr-verbs
#' @export
cbind_cols <- function(.data, ..., elements = "all") {
  cbind_factor(.data, ..., .matrix = "cols", elements = "all")
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
