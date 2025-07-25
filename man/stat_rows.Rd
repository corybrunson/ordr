% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-matrix.r
\name{stat_rows}
\alias{stat_rows}
\alias{stat_cols}
\title{Render plot elements for one matrix of an ordination}
\usage{
stat_rows(
  mapping = NULL,
  data = data,
  geom = "point",
  position = "identity",
  subset = NULL,
  elements = "active",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_cols(
  mapping = NULL,
  data = data,
  geom = "axis",
  position = "identity",
  subset = NULL,
  elements = "active",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
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

\item{subset}{An integer, logical, or character vector indicating a subset of
rows or columns for which to render graphical elements. NB: Internally, the
\code{subset} will be taken from the rows of the \link[=fortify.tbl_ord]{fortified}
'tbl_ord' comprising rows from only one of the matrix factors. It is still
possible to pass a formula to the \code{data} parameter, but it will act on the
fortified data \emph{before} it has been restricted to one matrix factor.}

\item{elements}{Character vector; which elements of each factor for which to
render graphical elements. One of \code{"all"} (the default), \code{"active"}, or any
supplementary element type defined by the specific class methods (e.g.
\code{"score"} for 'factanal', 'lda_ord', and 'cancord_ord' and \code{"intraset"} and
\code{"interset"} for 'cancor_ord').}

\item{...}{Additional arguments passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A ggproto \link[ggplot2:layer]{layer}.
}
\description{
These stats merely tell \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} which factor of an
ordination to pull data from for a plot layer. They are invoked internally
by the various \code{\link[=biplot-geoms]{geom_*_*()}} layers.
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

\examples{
# FA of Swiss social data
swiss_fa <-
  ordinate(swiss, model = factanal, factors = 2L, scores = "regression")
# active and supplementary elements
get_rows(swiss_fa, elements = "active")
head(get_rows(swiss_fa, elements = "score"))

# biplot using element filters and selection
# (note that filter precedes selection)
ggbiplot(swiss_fa) +
  geom_rows_point(elements = "score") +
  geom_rows_label(aes(label = name), elements = "score", subset = c(1, 4, 18)) +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  geom_cols_vector(aes(label = name))
}
\seealso{
Other biplot layers: 
\code{\link{biplot-geoms}},
\code{\link{biplot-stats}}
}
\concept{biplot layers}
