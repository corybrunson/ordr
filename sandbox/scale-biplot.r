
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

bi_axis <- function(
  name = derive(), breaks = waiver(), labels = waiver(),
  scale = ...
) {
  if (!is.formula(trans)) {
    stop("transformation for secondary axes must be a formula", call. = FALSE)
  }
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
  position = "bottom", sec.axis = bi_axis()
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
  if (! is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (! is.sec_axis(sec.axis)) {
      stop("Secondary axes must be specified using 'sec_axis()'")
    }
    sc$secondary.axis <- sec.axis
  }
  sc
}
