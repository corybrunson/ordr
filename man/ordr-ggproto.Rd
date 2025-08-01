% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa-.r, R/coord-scaffold.r,
%   R/geom-interpolation.r, R/geom-origin.r, R/stat-matrix.r,
%   R/stat-projection.r, R/zzz-biplot-stats.r
\docType{data}
\name{ordr-ggproto}
\alias{ordr-ggproto}
\alias{CoordScaffold}
\alias{GeomInterpolation}
\alias{GeomOrigin}
\alias{GeomUnitCircle}
\alias{StatRows}
\alias{StatCols}
\alias{StatProjection}
\alias{StatRowsDensity2d}
\alias{StatColsDensity2d}
\alias{StatRowsDensity2dFilled}
\alias{StatColsDensity2dFilled}
\alias{StatRowsEllipse}
\alias{StatColsEllipse}
\alias{StatRowsCenter}
\alias{StatColsCenter}
\alias{StatRowsStar}
\alias{StatColsStar}
\alias{StatRowsChull}
\alias{StatColsChull}
\alias{StatRowsPeel}
\alias{StatColsPeel}
\alias{StatRowsCone}
\alias{StatColsCone}
\alias{StatRowsDepth}
\alias{StatColsDepth}
\alias{StatRowsDepthFilled}
\alias{StatColsDepthFilled}
\alias{StatRowsScale}
\alias{StatColsScale}
\alias{StatRowsSpantree}
\alias{StatColsSpantree}
\alias{StatRowsBagplot}
\alias{StatColsBagplot}
\alias{StatRowsRule}
\alias{StatColsRule}
\alias{StatRowsProjection}
\alias{StatColsProjection}
\title{ggproto classes created and adapted for ordr}
\description{
In addition to geometric element layers (geoms) based on
base-\strong{ggplot2} layers like \code{geom_point()} but specified to matrix factors
as \code{geom_row_point()}, \strong{ordr} introduces \link[ggplot2:ggproto]{ggproto}
classes for some additional geometric elements commonly used in biplots.
The factor-specific geoms invoke the statistical transformation layers
(stats) \code{stat_rows()} and \code{stat_cols()}, which specify the matrix factor.
Because each ggplot layer consists of only one stat and one geom, this
necessitates that ggproto classes for new stats must also come in \verb{*Rows}
and \verb{*Cols} flavors.
}
\seealso{
\code{\link[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}} and \link[ggplot2:ggproto]{ggplot2::ggproto} for explanations
of base ggproto classes in \strong{ggplot2} and how to create new ones.
}
\keyword{datasets}
