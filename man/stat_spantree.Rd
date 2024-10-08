% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-spantree.r
\name{stat_spantree}
\alias{stat_spantree}
\title{Calculate a minimum spanning tree among cases or variables}
\usage{
stat_spantree(
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

\item{engine}{A single character string specifying the package implementation
to use; \code{"mlpack"}, \code{"vegan"}, or \code{"ade4"}.}

\item{method}{Passed to \code{\link[stats:dist]{stats::dist()}} if \code{engine} is \code{"vegan"} or \code{"ade4"},
ignored if \code{"mlpack"}.}

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
}
\value{
A ggproto \link[ggplot2:layer]{layer}.
}
\description{
This stat layer identifies the \eqn{n-1} pairs among \eqn{n}
points that form a minimum spanning tree, then calculates the segments
between these poirs in the two dimensions \code{x} and \code{y}.
}
\details{
A minimum spanning tree (MST) on the point cloud \eqn{X} is a minimal
connected graph on \eqn{X} with the smallest possible sum of distances (or
dissimilarities) between linked points. These layers call \code{\link[stats:dist]{stats::dist()}} to
calculate a distance/dissimilarity object and an engine from \strong{mlpack},
\strong{vegan}, or \strong{ade4} to calculate the MST. The result is formatted with
position aesthetics readable by \code{\link[ggplot2:geom_segment]{ggplot2::geom_segment()}}.

An MST calculated on \code{x} and \code{y} reflects the distances among the points in
\eqn{X} in the reduced-dimension plane of the biplot. In contrast, one
calculated on the full set of coordinates reflects distances in
higher-dimensional space. Plotting this high-dimensional MST on the
2-dimensional biplot provides a visual cue as to how faithfully two
dimensions can encapsulate the "true" distances between points (Jolliffe,
2002).
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
\dontrun{
# classical multidimensional scaling of road distances between European cities
euro_mds <- ordinate(eurodist, cmdscale_ord, k = 11)

# biplot with minimal spanning tree based on full-dimensional distances
# (as implemented in {mlpack})
euro_mds \%>\%
  negate_ord("PCo2") \%>\%
  ggbiplot() +
  stat_cols_spantree(
    ord_aes(euro_mds), engine = "mlpack",
    alpha = .5, linetype = "dotted"
  ) +
  geom_cols_text(aes(label = name), size = 3) +
  ggtitle(
    "MDS biplot of road distances between European cities",
    "Dotted segments constitute the minimal spanning tree"
  )
}
}
\references{
Jolliffe IT (2002) \emph{Principal Component Analysis}, Second Edition.
Springer Series in Statistics, ISSN 0172-7397. \doi{10.1007/b98835}
\url{https://link.springer.com/book/10.1007/b98835}
}
\seealso{
Other stat layers: 
\code{\link{stat_center}()},
\code{\link{stat_chull}()},
\code{\link{stat_cone}()},
\code{\link{stat_scale}()}
}
\concept{stat layers}
