#' @section Referential stats:

#' This statistical transformation is done with respect to reference data passed
#' to `referent` (ignored if `NULL`, the default, possibly resulting in empty
#' output). See [gggda::stat_referent()] for more details. This relies on a
#' sleight of hand through a new undocumented `LayerRef` class and associated
#' [ggplot2::ggplot_add()] method. As a result, only layers constructed using
#' this `stat_*()` shortcut will pass the necessary positional aesthetics to the
#' `$setup_params()` step, making them available to pre-process `referent` data.
#'
#' The biplot shortcuts automatically substitute the complementary matrix factor
#' for `referent = NULL` and will use an integer vector to select a subset from
#' this factor. These uses do not require the mapping passage.
#' 
