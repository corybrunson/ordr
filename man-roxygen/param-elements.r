#' @param elements Character vector; which elements of each factor for which to
#'   render graphical elements. One of `"active"` (the default) or any
#'   supplementary element type defined by the specific class methods (e.g.
#'   `"score"` for 'factanal', 'lda_ord', and 'cancord_ord' and `"intraset"` and
#'   `"interset"` for 'cancor_ord'), via [partial matching][base::match.arg()].
