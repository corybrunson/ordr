% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz-biplot-stats.r
\name{biplot-stats}
\alias{biplot-stats}
\alias{stat_rows_ellipse}
\alias{stat_cols_ellipse}
\alias{stat_rows_center}
\alias{stat_cols_center}
\alias{stat_rows_star}
\alias{stat_cols_star}
\alias{stat_rows_chull}
\alias{stat_cols_chull}
\alias{stat_rows_cone}
\alias{stat_cols_cone}
\alias{stat_rows_scale}
\alias{stat_cols_scale}
\alias{stat_rows_spantree}
\alias{stat_cols_spantree}
\title{Convenience stats for row and column matrix factors}
\usage{
stat_rows_ellipse(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  type = "t",
  level = 0.95,
  segments = 51,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_cols_ellipse(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  type = "t",
  level = 0.95,
  segments = 51,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_rows_center(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.min = NULL,
  fun.max = NULL,
  fun.args = list()
)

stat_cols_center(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.min = NULL,
  fun.max = NULL,
  fun.args = list()
)

stat_rows_star(
  mapping = NULL,
  data = NULL,
  geom = "segment",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.args = list()
)

stat_cols_star(
  mapping = NULL,
  data = NULL,
  geom = "segment",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  fun.data = NULL,
  fun.center = NULL,
  fun.args = list()
)

stat_rows_chull(
  mapping = NULL,
  data = NULL,
  geom = "polygon",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_cols_chull(
  mapping = NULL,
  data = NULL,
  geom = "polygon",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_rows_cone(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  origin = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_cols_cone(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  origin = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_rows_scale(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  mult = 1
)

stat_cols_scale(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  mult = 1
)

stat_rows_spantree(
  mapping = NULL,
  data = NULL,
  geom = "segment",
  position = "identity",
  engine = "mlpack",
  method = "euclidean",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_cols_spantree(
  mapping = NULL,
  data = NULL,
  geom = "segment",
  position = "identity",
  engine = "mlpack",
  method = "euclidean",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{The geometric object to use to display the data for this layer.
When using a \verb{stat_*()} function to construct a layer, the \code{geom} argument
can be used to override the default coupling between stats and geoms. The
\code{geom} argument accepts the following:
\itemize{
\item A \code{Geom} ggproto subclass, for example \code{GeomPoint}.
\item A string naming the geom. To give the geom as a string, strip the
function name of the \code{geom_} prefix. For example, to use \code{geom_point()},
give the geom as \code{"point"}.
\item For more information and other ways to specify the geom, see the
\link[ggplot2:layer_geoms]{layer geom} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Additional arguments passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

\item{type}{The type of ellipse.
The default \code{"t"} assumes a multivariate t-distribution, and
\code{"norm"} assumes a multivariate normal distribution.
\code{"euclid"} draws a circle with the radius equal to \code{level},
representing the euclidean distance from the center.
This ellipse probably won't appear circular unless \code{coord_fixed()} is applied.}

\item{level}{The level at which to draw an ellipse,
or, if \code{type="euclid"}, the radius of the circle to be drawn.}

\item{segments}{The number of segments to be used in drawing the ellipse.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{fun.data, fun.center, fun.min, fun.max, fun.args}{Functions and arguments
treated as in \code{\link[ggplot2:stat_summary]{ggplot2::stat_summary()}}, with \code{fun.center}, \code{fun.min}, and
\code{fun.max} behaving as \code{fun.y}, \code{fun.ymin}, and \code{fun.ymax}.}

\item{origin}{Logical; whether to include the origin with the transformed
data. Defaults to \code{FALSE}.}

\item{mult}{Numeric value used to scale the coordinates.}

\item{engine}{A single character string specifying the package implementation
to use; \code{"mlpack"}, \code{"vegan"}, or \code{"ade4"}.}

\item{method}{Passed to \code{\link[stats:dist]{stats::dist()}} if \code{engine} is \code{"vegan"} or \code{"ade4"},
ignored if \code{"mlpack"}.}
}
\value{
A ggproto \link[ggplot2:layer]{layer}.
}
\description{
These statistical transformations (stats) adapt
conventional \strong{ggplot2} stats to one or the other matrix factor
of a tbl_ord, in lieu of \code{\link[=stat_rows]{stat_rows()}} or \code{\link[=stat_cols]{stat_cols()}}. They
accept the same parameters as their corresponding conventional
stats.
}
\section{Ordination aesthetics}{

The convenience function \code{\link[=ord_aes]{ord_aes()}} can be used to incorporate all
coordinates of the ordination model into a statistical transformation. It
maps the coordinates to the custom aesthetics \code{..coord1}, \code{..coord2}, etc.

Some transformations, e.g. \code{\link[=stat_center]{stat_center()}}, are commutative with projection
to the 'x' and 'y' coordinates. If they detect aesthetics of the form
\verb{..coord[0-9]+}, then \code{..coord1} and \code{..coord2} are converted to \code{x} and \code{y}
while any remaining are ignored.

Other transformations, e.g. \code{\link[=stat_spantree]{stat_spantree()}}, yield different results in a
planar biplot when they are computer before or after projection. If such a
stat layer detects these aesthetics, then the lot of them are used in the
transformation.

In either case, the stat layer returns a data frame with position aesthetics
\code{x} and \code{y}.
}

\examples{
# compute row-principal components of scaled iris measurements
iris[, -5] \%>\%
  prcomp(scale = TRUE) \%>\%
  as_tbl_ord() \%>\%
  mutate_rows(species = iris$Species) \%>\%
  print() -> iris_pca

# row-principal biplot with centroids and confidence elliptical disks
iris_pca \%>\%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  geom_rows_point() +
  geom_polygon(
    aes(fill = species),
    color = NA, alpha = .25, stat = "rows_ellipse"
  ) +
  geom_cols_vector(color = "#444444") +
  scale_color_brewer(
    type = "qual", palette = 2,
    aesthetics = c("color", "fill")
  ) +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris measurements",
    "Overlaid with 95\% confidence disks"
  )
}
\seealso{
Other biplot layers: 
\code{\link{biplot-geoms}},
\code{\link{stat_rows}()}
}
\concept{biplot layers}
