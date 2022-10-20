# ordr 0.1.1

## `linewidth` aesthetic (breaking change)

An upcoming release of *ggplot2* controls stroke width using the new `linewidth` aesthetic rather than `size`. This *ordr* release adapts to this change internally for `GeomUnitCircle` and by updating row and column layers automatically adapted from *ggplot2*. (#50)

## class 'eigen'

Methods are added for the 'eigen' class returned by `eigen()`, and the internal `eigen_ord()` function and its methods are updated accordingly.

## list tidiers (breaking change)

The previous version extended the 'list' method for *broom*'s `tidy()` and `glance()` functions to recognize the output of `cmdscale()` and of `cancor()`.
These have been spun off to a nascent helper package, *broom.list*, to reduce size and prevent attachment warnings.

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
