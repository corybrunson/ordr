## Test environments

* local OS X install, R 4.1.1, "Kick Things" (via `devtools::check()`)
* local OS X install, R 4.1.3, "One Push-Up" (via `devtools::check()`)
* R-hub (via `rhub::check_for_cran()`)
* R-hub (via `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`)
* Win-Builder (devel, current, and previous; via `devtools::check_win_*()`)

### R CMD check results (R 4.1.1)

There were no ERRORs or NOTEs. There was one WARNING.

The warning read as follows:
```
   Found the following significant warnings:
     Warning: package ‘ggplot2’ was built under R version 4.1.2
   See ‘/private/var/folders/4p/3cy0qmp15x9216qsqhh84kzm0000gn/T/RtmpcTiMNE/ordr.Rcheck/00install.out’ for details.
```
To my knowledge, this cannot be addressed by amending this package.

### R CMD check results (R 4.1.3)

There were no ERRORs or WARNINGs. There was one NOTE.

The note read as follows:
```
Package suggested but not available for checking: ‘mlpack’
```
This was an intentional check on a machine without this package installed.

### Win-Builder

There were no ERRORs or WARNINGs. There was one NOTE.

The note flagged this as a new submission.
The note also flagged the possible misspellings 'biplot' (and its plural 'biplots'), 'decompositions', and 'scatterplots', which are standard terms in multivariate statistics.
Finally, the note flagged several URLs:
* <http://pascal-francis.inist.fr/vibad/index.php?action=getRecordDetail&idt=1158762>
* <https://doi.org/10.1111/j.1475-4754.2006.00270.x>
* <https://doi.org/10.2307/2394164>
* <http://doi.org/10.2307/2683520>
All of these URLs are correct and have been verified. (The DOI links redirect but are intended to remain DOI links.)

### R-hub

There were no ERRORs or WARNINGs. There was one NOTE.

The note read as follows:
```
Found the following files/directories:
  'lastMiKTeXException'
```
As documented, this note is probably due to a MiKTeX bug that can be ignored:
<https://github.com/r-hub/rhub/issues/503>

### R-hub (High Sierra)

## Reverse dependencies

This is a new submission with no reverse dependencies.
