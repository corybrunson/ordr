% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ord-augmentation.r
\name{augmentation}
\alias{augmentation}
\alias{augmentation_rows}
\alias{augmentation_cols}
\alias{augmentation_factor}
\alias{augmentation_coord}
\alias{augment_ord}
\title{Augment factors and coordinates of 'tbl_ord' objects}
\usage{
augmentation_rows(x)

augmentation_cols(x)

augmentation_factor(x, .matrix)

augmentation_coord(x)

augment_ord(x, .matrix = "dims")
}
\arguments{
\item{x}{An object of class '\link{tbl_ord}'.}

\item{.matrix}{A character string matching one of several indicators for one
or both matrices in a matrix decomposition used for ordination. The
standard values are \code{"rows"}, \code{"cols"}, and \code{"dims"} (for both).}
}
\description{
These functions return data associated with the cases,
variables, and coordinates of an ordination object, and attach it to the
object.
}
\details{
The \verb{augmentation_*()} methods produce \link[tibble:tibble]{tibble}s of values
associated with the rows, columns, and artificial coordinates of an object of
class '\link{tbl_ord}'. The first field of each tibble is \code{.name}, which contains
the row, column, or coordinate names. Additional fields contain information
about the rows, columns, or coordinates extracted from the ordination object.

The function \code{augment_ord()} returns the ordination with either or both
matrix factors annotated with the result of \verb{augmentation_*()}. In this way
\code{augment_ord()} works like \code{\link[generics:augment]{generics::augment()}}, as popularized by the
\strong{broom} package, by extracting information about the rows and columns, but
it differs in returning an annotated 'tbl_ord' rather than a
\link[tibble:tbl_df-class]{'tbl_df'} object. The advantage of implementing separate
methods for the rows, columns, and artificial coordinates is that more
information contained in the original object becomes accessible to the user.
}
\seealso{
\link{tidiers} and \link{annotation} methods that interface with augmentation.
}
