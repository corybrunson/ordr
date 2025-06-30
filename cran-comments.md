## Resubmission

This patch prepares for an upcoming major version increment that addresses
(1) an upcoming {ggplot2} upgrade and
(2) the upcoming spinoff and dependency {gggda}.

## R CMD checks

Local checks were performed both with the current CRAN version 3.5.2 of {ggplot2} and with the current development version (2025 Jun 30).

### Test environments

* local OS X install, R 4.4.2
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`
