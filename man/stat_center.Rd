% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-center.r
\name{stat_center}
\alias{stat_center}
\alias{stat_star}
\title{Compute geometric centers and spreads for ordination factors}
\usage{
stat_center(
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

stat_star(
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

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Additional arguments passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

\item{fun.data, fun.center, fun.min, fun.max, fun.args}{Functions and arguments
treated as in \code{\link[ggplot2:stat_summary]{ggplot2::stat_summary()}}, with \code{fun.center}, \code{fun.min}, and
\code{fun.max} behaving as \code{fun.y}, \code{fun.ymin}, and \code{fun.ymax}.}
}
\value{
A ggproto \link[ggplot2:layer]{layer}.
}
\description{
Compute geometric centers and spreads for ordination factors
}
\section{Biplot layers}{

\code{\link[=ggbiplot]{ggbiplot()}} uses \code{\link[ggplot2:fortify]{ggplot2::fortify()}} internally to produce a single data
frame with a \code{.matrix} column distinguishing the subjects (\code{"rows"}) and
variables (\code{"cols"}). The stat layers \code{stat_rows()} and \code{stat_cols()} simply
filter the data frame to one of these two.

The geom layers \verb{geom_rows_*()} and \verb{geom_cols_*()} call the corresponding
stat in order to render plot elements for the corresponding factor matrix.
\verb{geom_dims_*()} selects a default matrix based on common practice, e.g.
points for rows and arrows for columns.
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
# scaled PCA of Anderson iris measurements
iris[, -5] \%>\%
  princomp(cor = TRUE) \%>\%
  as_tbl_ord() \%>\%
  mutate_rows(species = iris$Species) \%>\%
  print() -> iris_pca

# row-principal biplot with centroid-based stars
iris_pca \%>\%
  ggbiplot(aes(color = species)) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 2) +
  stat_rows_star(alpha = .5, fun.center = "mean") +
  geom_rows_point(alpha = .5) +
  stat_rows_center(fun.center = "mean", size = 4, shape = 1L) +
  ggtitle(
    "Row-principal PCA biplot of Anderson iris measurements",
    "Segments connect each observation to its within-species centroid"
  )
}
\seealso{
Other stat layers: 
\code{\link{stat_chull}()},
\code{\link{stat_cone}()},
\code{\link{stat_scale}()},
\code{\link{stat_spantree}()}
}
\concept{stat layers}
