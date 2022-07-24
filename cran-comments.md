## Test environments

* local OS X install, R 4.1.1, "Kick Things" (via `devtools::check()`)
* R-hub (via `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`)
* Win-Builder (devel, current, and previous; via `devtools::check_win_*()`)

### R CMD check results

There were no ERRORs or NOTEs.

There was one WARNING, that "package ‘ggplot2’ was built under R version 4.1.2".
{ordr} Depends on {ggplot2}.

### Win-Builder

There were no ERRORs or WARNINGs.

There were two NOTEs: one flagging this as a new submission, ...

The check flagged the possible misspellings 'biplot' (and its plural 'biplots'), 'decompositions', and 'scatterplots', which are standard terms in multivariate statistics.

### R-hub

There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies

This is a new submission with no reverse dependencies.
