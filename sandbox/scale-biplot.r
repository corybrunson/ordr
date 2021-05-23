
# `ggbiplot(sec.axis = "rows")`
# This attempt begins with the `ggbiplot` call, from which affected layers must
# draw.

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  sec.axes = NULL,
  ...
) {
  # fortify `ordination` if necessary
  ordination <- fortify(ordination, coord.only = FALSE)
  
  # augment `mapping`, if necessary, with default coordinates
  mapping <- ordinate_aes(ordination, mapping)
  
  # if `sec.axes` is specified, then fortify `ordination` and scale the
  # secondary axis coordinates to match the primary axis
  if (! is.null(sec.axes)) {
    
    sec.axes <- match_factor(sec.axes)
    if (! sec.axes %in% c("rows", "cols")) {
      stop("Select one matrix factor, 'rows' or 'cols', ",
           "to scale to secondary axes.")
    }
    pri.axes <- setdiff(c("rows", "cols"), sec.axes)
    
    .coords <- str_sub(as.character(mapping[c("x", "y")]), start = 2)
    
    p_lim <- apply(ordination[ordination$.matrix == pri.axes, .coords], 2, max)
    s_lim <- apply(ordination[ordination$.matrix == sec.axes, .coords], 2, max)
    scale_factor <- min(p_lim / s_lim)
    
    ordination <- mutate_at(
      ordination,
      vars(.coords),
      funs(ifelse(.matrix == sec.axes, . * scale_factor, .))
    )
    
  }
  
  # conventional `ggplot()` call
  p <- ggplot(
    data = ordination,
    mapping = mapping,
    environment = parent.frame(),
    ...
  )
  # .matrix aesthetic indicating whether to plot cases or variables
  .matrix_aes <- list(.matrix = rlang::quo(!! rlang::sym(".matrix")))
  class(.matrix_aes) <- "uneval"
  p$mapping <- c(p$mapping, .matrix_aes)
  
  # if `sec.axes` is specified, then add secondary axes
  if (! is.null(sec.axes)) {
    p <- p + scale_x_continuous(sec.axis = sec_axis(~ . / scale_factor))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~ . / scale_factor))
  }
  
  # synchronize the scales AND BREAKS of the axes
  p$coordinates <- coord_equal()
  
  # add class label for potential future use
  class(p) <- c("ggbiplot", class(p))
  
  p
}


# `scale_*_continuous()`
# This is an alternate approach to the one below, introducing both `x` and `y`
# scales for one of the matrix factors.

scale_u_continuous <- function(
  name = waiver(), breaks = waiver(),
  minor_breaks = waiver(), labels = waiver(),
  limits = NULL, expand = waiver(), oob = scales::censor,
  na.value = NA_real_, trans = "identity",
  position = "bottomleft", sec.axis = waiver()
) {
  print(ls())
  save(list = ls(), file = "temp.RData")
  load("temp.RData")
  
  # extract positions
  position <- match.arg(position,
                        c("bottomleft", "topleft", "topright", "bottomright"))
  position_x <- stringr::str_extract(position, "^bottom|top")
  position_y <- stringr::str_extract(position, "left|right$")
  
  # calculate shared breaks
  #breaks <- 
  #minor_breaks <- 
  #labels <- 
  
  sc_x <- continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept",
      "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position_x, super = ScaleContinuousPosition
  )
  sc_y <- continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept",
      "ymin_final", "ymax_final", "lower", "middle", "upper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position_y, super = ScaleContinuousPosition
  )
  
  #if (!is.waive(sec.axis)) {
  #  if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
  #  if (!is.sec_axis(sec.axis))
  #    stop("Secondary axes must be specified using `sec_axis()`")
  #  sc$secondary.axis <- sec.axis
  #}
  
  # CANNOT ADD "ggproto" OBJECTS
}

# the ranges of 'U' and 'V' need to be recovered downstream of `ggbiplot()`
# the solution probably lies with "contexts"
# follow `dplyr::n()` down the rabbit hole


# `biplot_sec_axis()`
# The below approach, using a new secondary axis function, fails because
# transforming the axis has no effect on the plot elements.

biplot_sec_axis <- function(
  name = derive(), breaks = waiver(), labels = waiver(),
  scale = 1
) {
  #if (! inherits(trans, "formula")) {
  #  stop("transformation for secondary axes must be a formula", call. = FALSE)
  #}
  trans <- as.formula(paste("~.*", scale))
  ggproto(
    NULL, AxisSecondary,
    trans = trans,
    name = name,
    breaks = breaks,
    labels = labels
  )
}

scale_x_biplot <- function(
  name = waiver(), breaks = waiver(),
  minor_breaks = waiver(), labels = waiver(),
  limits = NULL, expand = waiver(), oob = scales::censor,
  na.value = NA_real_, trans = "identity",
  position = "bottom", sec.axis = biplot_sec_axis(scale = 5)
) {
  print(ls())
  save(list = setdiff(ls(), "sec.axis"), file = "temp.RData")
  load("temp.RData")
  
  sc <- continuous_scale(
    c(
      "x", "xmin", "xmax", "xend",
      "xintercept", "xmin_final", "xmax_final",
      "xlower", "xmiddle", "xupper"
    ),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = ScaleContinuousPosition
  )
  
  set_sec_axis(sec.axis, sc)
  
}
