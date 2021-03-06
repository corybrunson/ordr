% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-axis.r
\name{geom_axis}
\alias{geom_axis}
\title{Render axes through origin}
\usage{
geom_axis(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
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

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Additional arguments passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

\item{na.rm}{Passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

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
\code{geom_axis()} renders lines through the origin and the position
of each case or variable.
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

\code{geom_axis()} understands the following aesthetics (required aesthetics are
in bold):
\itemize{
\item \strong{\code{x}}
\item \strong{\code{y}}
\item \code{alpha}
\item \code{colour}
\item \code{linetype}
\item \code{size}
\item \code{group}
}
}

\examples{
# Reaven & Miller overt & chemical diabetes test data and group classification
head(heplots::Diabetes)
# default (standardized) linear discriminant analysis of groups on tests
diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes)
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(diabetes_lda) \%>\%
  augment_ord() \%>\%
  mutate_rows(discriminant = ifelse(! .supplement, "centroid", "case")) \%>\%
  print() -> diabetes_lda
# row-standard biplot
diabetes_lda \%>\%
  confer_inertia(1) \%>\%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_rows_point(aes(shape = .grouping, size = discriminant), alpha = .5) +
  geom_cols_axis(color = "#888888") +
  geom_cols_axis_ticks(num = 8L) +
  geom_cols_axis_text(size = 2.5, num = 8L, label_dodge = .2) +
  geom_cols_axis_label(aes(label = .name)) +
  ggtitle(
    "LDA of Reaven & Miller diabetes groups",
    "Row-standard biplot of standardized LDA"
  )
# contribution LDA of groups on tests
diabetes_lda <-
  lda_ord(group ~ ., heplots::Diabetes, axes.scale = "contribution")
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(diabetes_lda) \%>\%
  augment_ord() \%>\%
  mutate_rows(discriminant = ifelse(! .supplement, "centroid", "case")) \%>\%
  print() -> diabetes_lda
# symmetric biplot
diabetes_lda \%>\%
  confer_inertia(.5) \%>\%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_rows_point(aes(shape = .grouping, alpha = discriminant)) +
  geom_cols_axis(color = "#888888") +
  geom_cols_axis_ticks(num = 8L) +
  geom_cols_axis_text(size = 2.5, num = 8L, label_dodge = .1) +
  ggtitle(
    "LDA of Reaven & Miller diabetes groups",
    "Symmetric biplot of contribution LDA"
  )
}
\seealso{
Other geom layers: 
\code{\link{geom_axis_label}()},
\code{\link{geom_axis_text}()},
\code{\link{geom_axis_ticks}()},
\code{\link{geom_isolines_text}()},
\code{\link{geom_isolines}()},
\code{\link{geom_lineranges}()},
\code{\link{geom_origin}()},
\code{\link{geom_text_radiate}()},
\code{\link{geom_unit_circle}()},
\code{\link{geom_vector}()}
}
\concept{geom layers}
