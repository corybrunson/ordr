#' Biplots following the grammar of graphics
#' 
#' Build a biplot visualization from ordination data wrapped as a
#' \code{tbl_ord}.
#' 

#' \code{ggbiplot} produces a \strong{\link[ggplot2]{ggplot2}} object from a 
#' \code{\link{tbl_ord}} object. The baseline object is the default unadorned
#' \code{ggplot} object \code{p} with the following differences:

#' \itemize{

#'   \item \code{p$mapping} is augmented with \code{.matrix = .matrix}, which
#'   expects either \code{.matrix = "u"} or \code{.matrix = "v"} from the biplot
#'   layers.

#'   \item \code{p$coordinates} is set to \code{\link[ggplot2]{coord_fixed}} in
#'   order to faithfully render the geometry of an ordination.

#'   \item \code{p} is assigned the class \code{"ggbiplot"}. This serves no
#'   functional purpose currently.

#' }

#' Furthermore, the user may feed single integer values to the \code{x} and
#' \code{y} aesthetics, which will be interpreted as the corresponding
#' coordinates in the ordination.
#'
#' \code{ord_aes} is a convenience function that generates a full-rank set of
#' coordinate aesthetics \code{.coord1}, \code{.coord2}, etc. mapped to the
#' shared coordinates of the ordination object, along with any additional
#' aesthetics that are processed internally by \code{\link[ggplot2]{aes}}.
#' 

#' @template ggbiplot-layers

#' @name ggbiplot
#' @import ggplot2
#' @param ordination A \code{\link{tbl_ord}}.
#' @param mapping List of default aesthetic mappings to use for the biplot. The
#'   default assigns the first two coordinates to the aesthetics \code{x} and
#'   \code{y}. Other assignments must be supplied in each layer added to the
#'   plot.
#' @param sec.axes Matrix factor character to specify a secondary set of axes.
#' @param scale.factor Numeric value used to scale the secondary axes against
#'   the primary axes; ignored if \code{sec.axes} is not specified.
#' @param scale_u,scale_v Either the character name of a numeric variable in
#'   \code{get_*(ordination)} or a numeric vector of length
#'   \code{nrow(get_*(ordination))}, used to scale the coordinates of \eqn{U} or
#'   \eqn{V}, respectively.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}} or to
#'   \code{\link[ggplot2]{aes}}.

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  sec.axes = NULL, scale.factor = NULL,
  scale_u = NULL, scale_v = NULL,
  ...
) {
  # fortify `ordination` if necessary
  ordination <- fortify(ordination, include = "all")
  
  # augment `mapping`, if necessary, with default coordinates
  mapping <- ensure_xy_aes(ordination, mapping)
  
  # scale 'U' or 'V' as indicated by `scale_u` and `scale_v`
  if (! is.null(scale_u)) {
    ordination <- scale_ord(ordination, "u", mapping, scale_u)
  }
  if (! is.null(scale_v)) {
    ordination <- scale_ord(ordination, "v", mapping, scale_v)
  }
  
  # if `sec.axes` is specified, then fortify `ordination` and
  # scale the secondary axis coordinates to match the primary axis
  if (! is.null(sec.axes)) {
    
    sec.axes <- match_factor(sec.axes)
    if (! sec.axes %in% c("u", "v")) {
      stop("Select one matrix factor, 'u' or 'v', to scale to secondary axes.")
    }
    pri.axes <- setdiff(c("u", "v"), sec.axes)
    
    if (is.null(scale.factor)) {
      ps_lim <- lapply(c(pri.axes, sec.axes), function(.m) {
        apply(
          # recover coordinates stored as attribute during `fortify()`
          ordination[ordination$.matrix == .m, get_coord(ordination)],
          2, function(x) c(min(x), max(x))
        )
      })
      scale.factor <- min(ps_lim[[1]] / ps_lim[[2]])
    }
    
    ordination <- dplyr::mutate_at(
      ordination,
      dplyr::vars(get_coord(ordination)),
      dplyr::funs(ifelse(ordination$.matrix == sec.axes, . * scale.factor, .))
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
    # -+-THIS APPROACH IS VULNERABLE TO DOWNSTREAM `x` AND `y` SCALES-+-
    p <- p + scale_x_continuous(sec.axis = sec_axis(~ . / scale.factor))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~ . / scale.factor))
  }
  
  # synchronize the scales -+-AND BREAKS-+- of the axes
  p$coordinates <- coord_fixed()
  
  # add class label for potential future use
  class(p) <- c("ggbiplot", class(p))
  
  p
}

# interpret numerical x and y coordinates as coordinates;
# assume first two coordinates if none are provided
ensure_xy_aes <- function(ordination, mapping) {
  coords <- get_coord(ordination)
  coord_vars <- syms(coords)
  if (is.null(mapping$y)) {
    mapping <- c(aes(y = !! coord_vars[[2]]), mapping)
  } else {
    if (is.numeric(mapping$y) && length(mapping$y) == 1) {
      mapping <- c(
        aes(y = !! coord_vars[[mapping$y]]),
        mapping[setdiff(names(mapping), "y")]
      )
    }
  }
  if (is.null(mapping$x)) {
    mapping <- c(aes(x = !! coord_vars[[1]]), mapping)
  } else {
    if (is.numeric(mapping$x) && length(mapping$x) == 1) {
      mapping <- c(
        aes(x = !! coord_vars[[mapping$x]]),
        mapping[setdiff(names(mapping), "x")]
      )
    }
  }
  class(mapping) <- "uneval"
  mapping
}

# use `.m` to avoid conflict with '.matrix' column in `ordination`
scale_ord <- function(ordination, .m, mapping, scale) {
  if (is.character(scale)) scale <- ordination[[scale]]
  dplyr::mutate_at(
    ordination,
    dplyr::vars(stringr::str_remove(as.character(mapping[c("x", "y")]), "^~")),
    dplyr::funs(ifelse(ordination$.matrix == .m, . * scale, .))
  )
}

#' @rdname ggbiplot
#' @export
ord_aes <- function(ordination, ...) {
  # process all coordinate aesthetics
  ord_aes <- lapply(
    get_coord(ordination),
    function(nm) rlang::quo(!! rlang::sym(nm))
  )
  names(ord_aes) <- paste0(".coord", seq_along(ord_aes))
  # process other aesthetics
  other_aes <- aes(...)
  # concatenate aesthetics
  aes <- c(ord_aes, other_aes)
  class(aes) <- "uneval"
  aes
}
