## Update after 4 years: summary of changes

This is an update of MFO 0.1.0 (CRAN, February 2022). Version 0.2.0 was
released on GitHub only, so CRAN users go from 0.1.0 to 0.2.1. The formulas
were checked against the primary sources and several were corrected; full
details are in NEWS.md.

The maintainer e-mail is unchanged.

* Breaking: `method = "MRER"` now implements the MRER of Cheneviere et al.
  (2009); the former behaviour (highest measured value) is `method = "MV"`.
  Results change with default arguments (0.2.0: third-degree polynomial by
  default; 0.2.1: VO2max as a 30-s rolling mean, corrected SIN model).
* Bug fixes: SIN model formula (eq. 14 of Cheneviere et al. 2009) and fit to
  the observed steps; resting values with the selected equation; heart rate
  no longer removes rows; coefficients of Peronnet and Massicotte (1991)
  derived from their published constants.
* New arguments, all with defaults that keep existing calls working:
  `include_basal`, `vo2max_window`, `energy_equivalents`, `fatmin_threshold`,
  `min_steps`, `use_incomplete_step`; named values for `RER_cutoff`.
* Data-quality warnings (few steps, Fatmax at the first or last step,
  incomplete last step, missing heart rate).
* New vignette, pkgdown site, inst/CITATION, and 94 % test coverage.
* Dependencies: `magrittr` and `stringr` are no longer imported; R >= 4.1.0
  is now required (was R >= 2.10).

## R CMD check results

0 errors | 0 warnings | 0 notes on the 5 GitHub Actions runners of the
package CI and on the 3 R-hub platforms. The NOTEs of the local check come
from the local machine only.

### Local

`R CMD check --as-cran` on the built tarball `MFO_0.2.1.tar.gz`:
macOS 15.4.1 (aarch64-apple-darwin), R 4.5.3.

0 errors | 0 warnings | 2 notes

* `checking for future file timestamps ... NOTE: unable to verify current
  time`: intermittent; the local machine cannot always reach the time
  server.
* `checking HTML version of manual ... NOTE: Skipping checking HTML
  validation: 'tidy' doesn't look like recent enough HTML Tidy`: the local
  machine has the old HTML Tidy shipped with macOS.

### GitHub Actions, package CI (`R CMD check --as-cran`)

| Runner         | Platform                 | R                                  | Result          |
|----------------|--------------------------|------------------------------------|-----------------|
| ubuntu-latest  | x86_64-pc-linux-gnu      | R-devel (2026-11-10 r90874)        | 0 / 0 / 0 notes |
| ubuntu-latest  | x86_64-pc-linux-gnu      | R 4.6.2 (release)                  | 0 / 0 / 0 notes |
| ubuntu-latest  | x86_64-pc-linux-gnu      | R 4.5.3 (oldrel-1)                 | 0 / 0 / 0 notes |
| macos-latest   | aarch64-apple-darwin23   | R 4.6.2 (release)                  | 0 / 0 / 0 notes |
| windows-latest | x86_64-w64-mingw32       | R 4.6.2 ucrt (release)             | 0 / 0 / 0 notes |

### R-hub v2 (`rhub::rhub_check()`, runs on GitHub Actions)

win-builder was unreachable when this release was prepared, so the
Windows and R-devel checks were run with R-hub v2, whose jobs run on
GitHub Actions in this repository (run "aerodynamic-fawn", on the same
commit as the submitted tarball; options `--no-manual --as-cran`; the PDF
manual is checked locally).

| R-hub platform | Platform                 | R                                  | Result          |
|----------------|--------------------------|------------------------------------|-----------------|
| windows        | x86_64-w64-mingw32       | R-devel (2026-11-10 r90874 ucrt)   | 0 / 0 / 0 notes |
| ubuntu-release | x86_64-pc-linux-gnu      | R 4.6.2 (Ubuntu 26.04.1 LTS)       | 0 / 0 / 0 notes |
| macos          | x86_64-apple-darwin20    | R-devel (2026-11-11 r90876)        | 0 / 0 / 0 notes |

## URL checks

`urlchecker::url_check()` reports HTTP 403 for
<https://doi.org/10.1080/17461391.2020.1788650> (README). The DOI is valid:
doi.org redirects to the publisher (Wiley Online Library), which returns
403 to non-browser clients. It opens normally in a browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
