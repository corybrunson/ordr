% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-stats-lm.r
\name{methods-lm}
\alias{methods-lm}
\alias{as_tbl_ord.lm}
\alias{recover_rows.lm}
\alias{recover_cols.lm}
\alias{recover_coord.lm}
\alias{augmentation_rows.lm}
\alias{augmentation_cols.lm}
\alias{augmentation_coord.lm}
\alias{augmentation_rows.glm}
\alias{recover_rows.mlm}
\alias{recover_cols.mlm}
\alias{recover_coord.mlm}
\alias{augmentation_rows.mlm}
\alias{augmentation_cols.mlm}
\alias{augmentation_coord.mlm}
\title{Functionality for linear model objects}
\usage{
\method{as_tbl_ord}{lm}(x)

\method{recover_rows}{lm}(x)

\method{recover_cols}{lm}(x)

\method{recover_coord}{lm}(x)

\method{augmentation_rows}{lm}(x)

\method{augmentation_cols}{lm}(x)

\method{augmentation_coord}{lm}(x)

\method{augmentation_rows}{glm}(x)

\method{recover_rows}{mlm}(x)

\method{recover_cols}{mlm}(x)

\method{recover_coord}{mlm}(x)

\method{augmentation_rows}{mlm}(x)

\method{augmentation_cols}{mlm}(x)

\method{augmentation_coord}{mlm}(x)
}
\arguments{
\item{x}{An ordination object.}
}
\description{
These methods extract data from, and attribute new data to,
objects of class \code{"lm"}, \code{"glm"}, and \code{"mlm"} as returned by \code{\link[stats:lm]{stats::lm()}}
and \code{\link[stats:glm]{stats::glm()}}.
}
\examples{
# Motor Trend design and performance data
head(mtcars)
# regression analysis of performance measures on design specifications
mtcars_centered <- scale(mtcars, scale = FALSE)
mtcars_centered \%>\%
  as.data.frame() \%>\%
  lm(formula = mpg ~ wt + cyl) \%>\%
  print() -> mtcars_lm
# wrap as a 'tbl_ord' object
(mtcars_lm_ord <- as_tbl_ord(mtcars_lm))
# summarize ordination
glance(mtcars_lm_ord)
# augment everything with names, predictors with observation stats
augment_ord(mtcars_lm_ord)
# calculate influences as the squares of weighted residuals
mutate_rows(augment_ord(mtcars_lm_ord), influence = .wt.res^2)
# regression biplot with performance isolines
mtcars_lm_ord \%>\%
  augment_ord() \%>\%
  mutate_cols(center = attr(mtcars_centered, "scaled:center")[.name]) \%>\%
  mutate_rows(influence = .wt.res^2) \%T>\% print() \%>\%
  ggbiplot(aes(x = wt, y = cyl, intercept = `(Intercept)`)) +
  #theme_biplot() +
  geom_origin(marker = "circle", radius = unit(0.02, "snpc")) +
  geom_rows_point(aes(color = influence)) +
  geom_cols_vector() +
  geom_cols_isolines(aes(center = center), by = .5) +
  geom_cols_isolines_text(aes(center = center), by = .5, label_dodge = -.15) +
  ggtitle(
    "Weight isolines with data colored by importance",
    "Regressing gas mileage onto weight and number of cylinders"
  )
}
