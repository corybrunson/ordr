## Resubmission: minor version 0.2.0

This minor version introduces a variety of fixes and upgrades, which incur some breaking changes.
It also outsources most of the straightforward {ggplot2} extensionality to {gggda}, now that the latter is published on CRAN.

## R CMD checks

Local checks were performed both with the current CRAN version 3.5.2 of {ggplot2} and with the current development version (2025 Jul 10) using `document = FALSE`.

### Test environments

* local OS X install, R 4.4.2
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

### local results

There were no ERRORs or WARNINGs.
Two NOTEs were consistently produced:

```
❯ checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘mlpack’

❯ checking for future file timestamps ... NOTE
  unable to verify current time
```

The first was an intentional check on a machine without these packages installed.
The second is presumably due to internet speeds.

### Win-Builder results

There were no ERRORs or WARNINGs. All NOTEs not seen above are addressed below.

The following URLs work for me:

```
Found the following (possibly) invalid URLs:
  URL: https://scholar.sun.ac.za/items/279f7958-0b54-43f1-8c75-da652f65db3f
    From: man/theme_scaffold.Rd
    Status: 403
    Message: Forbidden
  URL: https://ww2.amstat.org/meetings/jsm/2022/
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://ww2.amstat.org/meetings/jsm/2022/onlineprogram/ActivityDetails.cfm?SessionID=222053
    From: README.md
    Status: 403
    Message: Forbidden
```
