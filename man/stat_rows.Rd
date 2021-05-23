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
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_cols(
  mapping = NULL,
  data = data,
  geom = "axis",
  position = "identity",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

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

\item{geom}{The geometric object to use display the data}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

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

\seealso{
Other biplot layers: 
\code{\link{biplot-geoms}},
\code{\link{biplot-stats}}
}
\concept{biplot layers}