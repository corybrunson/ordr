# next version

## upgrades to the biplot chassis

### axis harmonizers

Multiple harmonizers are now available for scaling secondary axes. That recommended by Gower, Gardner--Lubbe, and Le Roux (2011) is the default, but the user can still specify a numeric scale instead.

### GDA-geared coordinate systems

Two new coordinate systems provide control over the aspect ratio of the plotting window without compromising that of the (artificial) coordinate axes:
`GeomRect` (alias `GeomSquare`) extends `GeomFixed` with an `window_ratio` parameter for the plotting window, while `GeomBiplot` removes the `ratio` parameter and forces the coordinate axes to have aspect ratio 1.

### scaffold theme

The 'biplot' theme has been renamed 'scaffold', with an alias for backward compatibility.

### standardized and restrictive elements parameter (breaking change)

The `elements` parameter is now standardized across all statistical transformations (through the code generation process) and accepts more restrictive options:
The value is argument-matched to `"active"`, `"score"`, or `"structure"`; these options may expand as additional supplementary elements are introduced.
Moreover, the former default `"all"` is no longer accepted, which forecloses the trick of passing the element type to an aesthetic, e.g. `size = .element == "active"`, as had been used in several examples.

## upgrades to existing plot layers

### combined vector and radiating text geom (breaking change)

The 'vector' and 'text_radiate' geoms have been combined.
The shortcut `geom_text_radiate()` is deprecated, and `geom_vector()` generates radiating labels by default.

### debugged axis geom

The 'axis' and 'isoline' geoms hit trouble when one or more points lay at the origin (`x^2 + y^2 == 0`). These cases have now been removed in `setup_data()`.

### reconciliation of summary functions

The 'center' and 'star' stats now follow the 'summary' stat convention of using `fun`, so `fun.center` is deprecated.
Additionally, `fun.ord` accepts a function that summarizes the columns of a matrix, which accommodates summaries like the depth median that do not decompose along orthogonal axes.

## new plot layers

### addition geom

A new 'interpolation' geometric element layer renders either of two methods of vector addition to interpolate the position---on the existing ordination and its biplot---of a new row or column of the original data matrix.

### referential stats

A new statistical transformation serves to parent specific "referential stats", meaning those that depend on non-inherited (in this setting, positional) data to transform the inherited data. The reference data are passed to the new `referent` parameter. The new stat is coupled with an additional `LayerRef` class that enables `ggplot_add()` to pass the inherited positional aesthetics to `$setup_params()`. Biplot-specific `stat_*_*()` shortcuts accept additional argument types to `referent` that result in the opposite matrix factor being used as reference data.

### projection stat

The 'projection' referential stat prepares segment endpoints between `x,y` positions and their projections on axes defined by the reference data. It is a natural graphical element for predictive biplots of ordination models of continuous data.

### rule stat

A new 'rule' statistical transformation computes additional position aesthetics that the 'axis' geom uses to limit and offset axes. The stat is referential and expects a set of functions that compute limits `lower` and `upper` along the axes and `yintercept` and `xintercept` associated with offset axes. The 'axis' geom preprocesses these aesthetics to rule endpoints `xmin,ymin,xmax,ymax` and offset vectors `xend,yend` to force the plotting window to contain the limited axis segments or, if the axes remain lines, the offsets where they are centered.

### peel stat

A new 'peel' statistical transformation computes nested convex hulls containing specified fractions of data.

### depth stat

A new 'depth' statistical transformation estimates depth across a grid and is paired with `GeomContour` to produce depth contours, which can be used to plot alpha bags.

### adapted density stat & geom

Aided by element standardization, the classic `density_2d` statistical transformation and geometric construction are adapted to biplots.
Currently, source code generation does not respect fixed parameters passed to `layer()` by the `stat_*()` and `geom_*()` shortcuts; as a consequence, `contour = TRUE` must be manually passed to `geom_*_density_2d()`.

# ordr 0.1.1

## `linewidth` aesthetic (breaking change)

An upcoming release of **ggplot2** controls stroke width using the new `linewidth` aesthetic rather than `size`. This **ordr** release adapts to this change internally for `GeomUnitCircle` and by updating row and column layers automatically adapted from **ggplot2**. (#50)

## class 'eigen'

Methods are added for the 'eigen' class returned by `eigen()`, and the internal `eigen_ord()` function and its methods are updated accordingly.

## list tidiers (breaking change)

The previous version extended the 'list' method for **broom**'s `tidy()` and `glance()` functions to recognize the output of `cmdscale()` and of `cancor()`.
These have been spun off to a nascent helper package, **broom.list**, to reduce size and prevent attachment warnings.

# ordr 0.1.0

In addition to minor changes, the following substantive changes are made from v0.0.2:

## augmentation

The column names of `augment_ord()` and `tidy()` outputs are no longer prefixed by periods.

## list tidiers

A new list tidier for `cancor()` output is introduced.

## negation

Tools to negate a subset of artificial dimensions, adapted from the sandboxed set of alignment tools, are provided.

## convenience function

The `ordinate()` function is now a generic with methods for four data classes.

## ggbiplot

The logical `prediction` parameter is replaced with the character-valued `axis.type`, based on the `ax.type` parameter used in **UBbipl** (Gower, Gardner--Lubbe, & le Roux, 2011).

## tests

Unit tests are overhauled to remove contexts, omit ad hoc tests, standardize method tests, and cover additional methods.

## documentation

The term 'accessor', used to refer to S3 class methods to recover standardized model components, has been changed to 'recoverer'.

# ordr 0.0.2

This pre-release makes several substantive changes in preparation for CRAN submission.

## element types

Annotation of active and supplementary elements is standardized: The character augmentation `.element` replaces the logical `.supplement`, and class methods either omit it entirely (when all retrieved elements are active) or include it in both row and column augmentation (when either includes supplementary elements). The value of `.element` is either 'active' or a type of supplement, e.g. 'score'.

## dependencies

Extraneous dependencies are dropped to reduce overhead. This takes two forms:

1. Methods for classes from specialty (lower-priority) packages (**candisc** and **ca**) are moved to **ordr.extra** and replaced with methods for classes from higher-priority packages (**stats** and **MASS**).
2. One-off uses of specialty packages for examples and vignettes are replaced.

## documentation

The `Description` field in DESCRIPTION is expanded and documentation throughout is slightly revised, including some new references.

# ordr 0.0.1

This is a pre-release in anticipation of a first CRAN submission.
Upgrades between this pre-release and CRAN submission will focus on addressing issues.
