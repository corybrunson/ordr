
scale_u_continuous <- function(
  name = waiver(), breaks = waiver(),
  minor_breaks = waiver(), labels = waiver(),
  limits = NULL, expand = waiver(), oob = censor,
  na.value = NA_real_, trans = "identity",
  position = "bottomleft", sec.axis = waiver()
) {
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
  
  # CANNOT ADD "ggproto" objects
}