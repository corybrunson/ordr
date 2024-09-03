
## Resubmission

This is a resubmission that addresses issues with two previous submissions flagged by the maintainers.
It also includes some substantive changes from the previous submission. The same set of checks have been performed and are mostly unchanged from the previous.

## Test environments

* local OS X install, R 4.1.1, "Kick Things" (via `devtools::check()`)
* local OS X install, R 4.2.1, "Funny-Looking Kid" (via `devtools::check()`) with **ggplot2** installed at commit a58b48c (v3.3.6.9000 following `linewidth` change)
* R-hub (via `rhub::check_for_cran()`)
* R-hub (via `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`)
* Win-Builder (devel, current, and previous; via `devtools::check_win_*()`)

### R CMD check results (R 4.1.1)

There were no ERRORs, WARNINGs, or NOTEs.

### R CMD check results (R 4.2.1)

There were no ERRORs or WARNINGs. There was one NOTE.

The note read as follows:
```
Packages suggested but not available for checking: 'mlpack', 'vegan'
```
This was an intentional check on a machine without these packages installed.

### Win-Builder

There were no ERRORs or WARNINGs. There was one NOTE.

The note flagged this as a new submission.
The note also flagged the possible misspellings 'al', 'pre', 'biplot' (and its plural 'biplots'), 'eigen', 'decompositions', 'workflows', and 'scatterplots', which are standard technical terms or abbreviations.
Finally, the note flagged some possibly invalid DOI-based and other URLs:
* <https://doi.org/10.1111/j.1475-4754.2006.00270.x>
* <https://doi.org/10.2307/2394164>
* <https://doi.org/10.2307/2683520>
* <https://support.qs.com/hc/en-gb/articles/360021876820-QS-Institution-Classifications>
All of these are correct and have been verified. (Some redirect but are intended to remain DOI links for permanence.)

The note was consistent across all three checks, though the specific spellings and URLs varied.

### R-hub (default)

There were no ERRORs or WARNINGs. There were two NOTEs.

One note flagged the same new submission status, possible misspellings, and possibly invalid URLs as the Win-Builder checks.

One note read as follows:
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As documented, this note is probably due to a MiKTeX bug that can be ignored:
<https://github.com/r-hub/rhub/issues/503>

### R-hub (High Sierra)

There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies

There are no reverse dependencies.
