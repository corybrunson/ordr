#' @title Transformations with respect to reference data
#'

#' @description Compute statistics with respect to a reference data set with
#' shared positional variables.
#' 

#' @details
#'
#' Often in geometric data analysis a statistical transformation applied to data
#' \eqn{X} will also depend on data \eqn{Y}, for example when drawing the
#' projections of vectors \eqn{X} onto vectors \eqn{Y}. The stat layer
#' `stat_referent()` accepts \eqn{Y} as an argument to the `referent` parameter
#' and pre-processes them using the existing positional aesthetic mappings to
#' `x` and `y`. (This requires a sleight of hand through a new undocumented
#' `LayerRef` class and associated [ggplot2::ggplot_add()] method.)
#'
#' The ggproto can be used as a parent to more elaborate statistical
#' transformations, or the stat can be paired with geoms that expect the
#' `referent` parameter and use it to position their transformations of \eqn{X}.
#' It pairs by default to `[ggplot2::geom_blank()]` so as to prevent possibly
#' confusing output.
#' 

#' @inheritParams ggplot2::layer
#' @template return-layer
#' @example inst/examples/ex-stat-referent.r
stat_referent <- function(
    mapping = NULL, data = NULL,
    geom = "blank", position = "identity",
    referent = NULL,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  LayerRef <- layer(
    data = data,
    mapping = mapping,
    stat = StatReferent,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      referent = referent,
      na.rm = FALSE,
      ...
    )
  )
  
  # undocumented class for custom `ggplot_add()` method
  class(LayerRef) <- c("LayerRef", class(LayerRef))
  LayerRef
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatReferent <- ggproto(
  "StatReferent", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {
    
    # map aesthetics from referent data, in current environment
    # required `x` and `y` aesthetics should be in `data`
    # (code adapted from `ggplot2:::Layer$compute_aesthetics()`)
    # NB: No checks are conducted here as in `$compute_aesthetics()`.
    if (! is.null(params$referent)) {
      params$mapping |> 
        lapply(rlang::eval_tidy, data = params$referent) |> 
        as.data.frame() ->
        params$referent
    }
    
    # discard combined mapping parameter
    params$mapping <- NULL
    
    params
  },
  
  compute_group = function(data, scales, referent = NULL) data
)

# QUESTION: Why are the arguments apparently out of order?
#' @rdname stat_referent
#' @export
ggplot_add.LayerRef <- function(object, plot, object_name) {
  
  # store global position mappings as a parameter
  object$stat_params$mapping <- plot$mapping[c("x", "y")]
  
  NextMethod()
}
