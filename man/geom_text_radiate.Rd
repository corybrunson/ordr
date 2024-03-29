% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-text-radiate.r
\name{geom_text_radiate}
\alias{geom_text_radiate}
\title{Text radiating outward from the origin}
\usage{
geom_text_radiate(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
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

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function. Cannot be jointly specified with
\code{nudge_x} or \code{nudge_y}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{parse}{If \code{TRUE}, the labels will be parsed into expressions and
displayed as described in \code{?plotmath}.}

\item{check_overlap}{If \code{TRUE}, text that overlaps previous text in the
same layer will not be plotted. \code{check_overlap} happens at draw time and in
the order of the data. Therefore data should be arranged by the label
column before calling \code{geom_text()}. Note that this argument is not
supported by \code{geom_label()}.}

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
}
\value{
A ggproto \link[ggplot2:layer]{layer}.
}
\description{
\code{geom_text_radiate()} is adapted from \code{ggbiplot()} in the
off-CRAN extensions of the same name (Vu, 2014; Telford, 2017; Gegzna,
2018). It renders text at specified positions and angles that radiate out
from the origin.
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

\section{Aesthetics}{

\code{geom_text_radiate()} understands the following aesthetics (required
aesthetics are in bold):
\itemize{
\item \strong{\code{x}}
\item \strong{\code{y}}
\item \strong{\code{label}}
\item \code{alpha}
\item \code{angle}
\item \code{colour}
\item \code{family}
\item \code{fontface}
\item \code{hjust}
\item \code{lineheight}
\item \code{size}
\item \code{vjust}
\item \code{group}
}
}

\references{
Vincent Q. Vu (2014). ggbiplot: A 'ggplot2' based biplot. R package version
0.55. \url{https://github.com/vqv/ggbiplot}, \code{experimental} branch

Richard J Telford (2017). ggbiplot: A 'ggplot2' based biplot. R package
version 0.6. \url{https://github.com/richardjtelford/ggbiplot} (fork),
\code{experimental} branch

Vilmantas Gegzna (2018). ggbiplot: A 'ggplot2' based biplot. R package
version 0.55. \url{https://github.com/forked-packages/ggbiplot} (fork), \code{experimental}
branch
}
\seealso{
Other geom layers: 
\code{\link{geom_axis}()},
\code{\link{geom_isoline}()},
\code{\link{geom_lineranges}()},
\code{\link{geom_origin}()},
\code{\link{geom_vector}()}
}
\concept{geom layers}
