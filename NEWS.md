# MFO (development version)

## Bug fixes

* `use_incomplete_step = TRUE` averaged only the last record of the
  incomplete step instead of its last `steady_state_duration` s, as
  documented (the start was counted from the nominal end of the step). In
  the example data cut to 66 records, step 8 VO2 goes from 2495 (one record)
  to 2444.7 ml/min (last 60 s). The default (`FALSE`) is not affected
  (SPEC 9.5).

# MFO 0.2.1

First CRAN release since 0.1.0 (2022). Scientific background and sources of
every formula in `docs/SPEC.md` (GitHub repository).

## Breaking changes

* `method = "MRER"` now implements the MRER of Chenevière et al. (2009,
  p. 1617): (1 − RER) and VO2 are fitted as straight lines of %VO2max and
  Fatmax is the vertex of their product, 1.7 (1 − RER) VO2. **In v0.2.0,
  `"MRER"` was a different method**: it returned the step with the highest
  observed 1.7 (1 − RER) VO2, a measured-values (MV) method. That behaviour
  is now `method = "MV"`, which uses the `author` equation (same step; MFO
  differs by the ratio of coefficients, e.g. 1.67/1.7 with Frayn). In the
  example data (Frayn) `"MRER"` moves from Fatmax 41.0 %VO2max /
  MFO 0.339 g/min to 35.8 %VO2max / 0.327 g/min; `"MV"` gives
  41.0 %VO2max / 0.327 g/min (SPEC 6.1, 19 finding 4).

* Results change with the default arguments. Because 0.2.0 never reached
  CRAN, users updating from 0.1.0 also get the 0.2.0 changes listed below
  (default `method = "P3"` instead of a second-degree polynomial). In 0.2.1:
  VO2max is a 30-s rolling mean, so Fatmax, the Fatmax zone and the
  crossover point (%VO2max) increase; the SIN parameters `d` and `t` follow
  Chenevière et al. (2009) and are not comparable with earlier values;
  `Fatmin` is the second zero of the SIN curve; `"Peronnet"` coefficients
  change in the 4th decimal; resting values use the `author` equation. See
  *Bug fixes* for the size of each change and how to reproduce 0.2.0.

## New features

* `method = "MV"` (measured values) in `MFO()` and `MFOs()`: Fatmax is the
  step with the highest observed FAT. `R_squared` and `RMSE` are `NA`; the
  Fatmax zone uses linear interpolation between steps and is labelled so in
  the plot and in the new `FatMax_zone_method` element (SPEC 6.1).

* `MFO()` and `MFOs()` gain `include_basal` (default `FALSE`). With `TRUE`
  the resting row enters `MFO_db` as step 0 and is part of the P3/P2 fit, as
  in Montes de Oca et al. (2021). The default reproduces v0.1.0/v0.2.0, which
  silently lost the resting row because its `HR` is `NA`; the exclusion is
  now explicit and no longer depends on missing HR. Example data, Frayn, P3:
  MFO 0.3167 -> 0.3225 g/min and Fatmax 36.17 -> 36.11 %VO2max with `TRUE`
  (SPEC 9.5).

* `RER_cutoff` accepts names from Amaro-Gahete et al. (2019): `"classic"`
  = 1.00, `"safe"` = 0.95, `"sedentary"` = 0.93, `"trained"` = 0.90.
  Numbers still work. Example data (Frayn, P3): 7, 6, 5 and 4 of 8 steps
  kept; Fatmax 33.9, 35.7, 36.9 and 29.5 %VO2max (SPEC 19, finding 2).

* `energy_equivalents` argument in `MFO()` and `MFOs()` for `Kcal` and
  `x_Kcal`: `"classic"` (4 kcal/g CHO, 9 kcal/g FAT, default in 0.2.x, as in
  v0.2.0) or `"source"`: Péronnet & Massicotte (1991, Table 1) 3.8683/9.7460
  kcal/g for `"Peronnet"` and, as a package decision, `"Frayn"` (Frayn 1983
  gives none); Jeukendrup & Wallis (2005, Table 5) CHO 3.95 kcal/g below
  50 %VO2max and 4.07 at or above, fat 9.75 kcal/g, for `"Jeukendrup"`
  (resting row: Péronnet & Massicotte values). MFO and Fatmax are not
  affected. In the example data (Frayn) `"source"` changes step `Kcal` by
  −1.9 % to +5.3 %. **The default will change to `"source"` in 0.3.0**; when
  the argument is not set, a message says so once per session
  (SPEC 19, finding 1).

* `MFO_kinetics()` and `MFOs()` gain `fatmin_threshold` (default `NULL`,
  Fatmin at the second zero of the SIN curve); a proportion of MFO such as
  0.05 gives the intensity above Fatmax where fat oxidation falls to that
  fraction, in closed form. The kinetics plot now shows the observed steps
  and draws the fitted curve between its two zeros (SPEC 19, finding 3).

* Quality warnings in `MFO()` and `MFOs()` (new section *Quality warnings*
  in `?MFO`): new argument `min_steps` (default 6) warns when fewer
  exercise steps enter the fit after `RER_cutoff` and
  `negative_FAT = "exclude"` (the resting row does not count); and a
  warning when Fatmax is at the edge, i.e. the exercise step nearest to
  Fatmax is the first or the last one (the test may have started too high
  or stopped too early). Results are unchanged. The example data give
  neither warning; with `RER_cutoff = "sedentary"` (5 steps) `min_steps`
  warns, and with `"trained"` (4 steps) MV, MRER and P2 also put Fatmax
  at the last step (SPEC 14).

* Incomplete last step: records left after the last complete step now always
  give a warning ("Last step incomplete (120 s of 180 s)"); v0.2.0 dropped
  them silently. The new `use_incomplete_step` argument of `MFO()` and
  `MFOs()` (default `FALSE`, results as in v0.2.0) uses that step only if
  it lasts at least `steady_state_duration`, averaging its last
  `steady_state_duration` s (SPEC 9.5).

* `MFOs()` re-issues every warning with the participant ID first
  (`"P02: No HR data in db_MFO: ..."`), so the quality warnings can be
  traced in a batch. The summary gains `FatMax_zone_method` and
  `energy_equivalents`, and `MFO()` returns `energy_equivalents`.

* `MFO()` returns `VO2max`, `VO2max_window` (effective, s) and
  `VO2max_window_requested`; `MFOs()` gains `vo2max_window` and the columns
  `VO2max` and `VO2max_window`.

## Bug fixes

* SIN model: `MFO_kinetics()` now implements equation [14] of Chenevière
  et al. (2009), `pi^(1/s) / (pi + 2*d)`; v0.1.0/v0.2.0 computed
  `pi^(1/s) / pi + 2*d`. Both describe the same family of curves, so the
  fitted curve can be the same, but `d` and `t` take different values: the
  old `d` was not Chenevière's dilatation and is not comparable with
  published values. With their Fig. 5 parameters (d = 0.18, s = 1.52,
  t = 0.21) the old formula put Fatmax at 28.9 %VO2max instead of 58.2
  (SPEC 6.5, 19 finding 5).

* SIN fitting procedure: `MFO_kinetics()` now fits the model directly to the
  observed steps (%MFO normalised to the highest observed step), as
  Chenevière et al. (2009) did. v0.1.0/v0.2.0 fitted a grid of ≥ 50 points
  predicted by a third-degree polynomial, which inflated n and shrank the
  standard errors of d, s and t. `FatMax_SIN` and `Fatmin` now come from
  closed forms derived from eq. [14], and `Fatmin` is the second zero of the
  curve, (π + d − t)/K, instead of the first grid point below 5 % of MFO.
  Example data (Frayn): d 0.144 -> −0.385, s 0.935 -> 0.910,
  t −0.093 -> 0.355, FatMax_SIN 36.0 -> 36.2, Fatmin 76.0 -> 76.4 %VO2max,
  R² 0.997 -> 0.965 (now on the observed steps); standard errors grow
  20–60 fold to their honest size. With `negative_FAT = "exclude"` or
  `RER_cutoff = 1` (Chenevière's step selection) the example gives
  s = 0.54 and Fatmin = 85.3 %VO2max: the SIN is sensitive to the last step,
  see `?MFO_kinetics` (SPEC 6.5, 19 findings 3 and 7).

* `MFO_kinetics()` now distinguishes two warnings: with fewer than 4 steps the
  SIN cannot be fitted (`convergence = FALSE`, as before); with 4 or 5 steps
  it is fitted but warns that the fit is unreliable and that at least 6 steps
  are recommended (SPEC 6.5).

* VO2max from `db_graded` is now the highest rolling mean over the new
  `vo2max_window` argument (default 30 s) instead of the single highest
  record. If the window is not a multiple of `step_time`, the number of
  records is rounded up (30 s with 20-s records = 2 records = 40 s) and a
  message reports the effective window. Fatmax, the Fatmax zone and the
  crossover point (%VO2max) increase by the ratio old/new VO2max; MFO (g/min)
  is unchanged. In the example data VO2max drops from 3442 to 3315 ml/min and
  Fatmax moves from 34.8 to 36.2 %VO2max. Use `vo2max_window = step_time` to
  reproduce v0.2.0 (SPEC 9.5, 14).

* Resting substrate oxidation (`x_CHO`, `x_FAT`, `x_Kcal`) now uses the
  same `author` equations as the test; it always used Frayn before.
  Jeukendrup & Wallis (2005) propose no resting equation: with
  `author = "Jeukendrup"` the package uses the 100 % glucose pair derived
  from their Table 4 constants (CHO = 4.58519 VCO2 − 3.22597 VO2,
  FAT = 1.69469 VO2 − 1.70130 VCO2), documented as a package decision.
  MFO and Fatmax are not affected because the resting row is not
  part of the fit. Example data with `"Peronnet"`: resting CHO
  0.130 -> 0.134 g/min, FAT 0.0726 -> 0.0723 g/min (SPEC 9.5).

* Péronnet & Massicotte (1991) print no oxidation equations: `"Peronnet"` now
  derives them in code from the O2/CO2 constants of their Table 1 (glucose
  0.7455/0.7426, fatty acid 2.0092/1.4136 L/g): CHO = 4.58519 VCO2 −
  3.22597 VO2, FAT = 1.69469 VO2 − 1.70130 VCO2, instead of the rounded
  4.585/3.2255/1.6946/1.7012. Changes are in the 4th decimal: example data,
  Fatmax 35.288 -> 35.285 %VO2max, MFO unchanged at 0.3158 g/min, resting CHO
  0.13369 -> 0.13361 g/min. Frayn (1983) prints his equations, which stay as
  printed (4.55/3.21; 1.67/1.67); their derivation from his Table 1 is a
  cross-check test (SPEC 19, finding 6).

* With `author = "Jeukendrup"`, `MFO()` now warns once when exercise steps
  fall outside 40–75 %VO2max, the range of the Jeukendrup & Wallis (2005,
  Table 5) equations, and lists their intensities; those steps were
  extrapolated silently before. Steps removed by `RER_cutoff` and the resting
  row never warn. Values are unchanged (SPEC 3, 9.5).

* Heart rate never decides which rows or steps are dropped. In v0.2.0 a test
  without an HR column made `MFO()` fail (`0 (non-NA) cases`; in `MFOs()`
  an empty row with no explanation), a step without HR was silently removed
  from the fit, and `read_MFO_databases()` deleted every row with a missing
  HR value, shifting all later steps (an empty HR column deleted the whole
  table). Now only VO2, VCO2 and RER remove rows, MFO and Fatmax are
  computed as usual, and a warning says that `FAT_MAX_HR` and
  `FAT_MAX_porcHRmax` are `NA` (no HR) or interpolated from fewer steps
  (some steps without HR, listed by %VO2max) (SPEC 9.5).

* When both `VO2max` and `db_graded` are given, `VO2max` is now used and
  `db_graded` is ignored with a warning (v0.2.0 silently used `db_graded`).

* Plots no longer cover the curves with text. `MFO_plot` shows FATmax, MFO
  and the method in the subtitle, explains the Fatmax zone and the crossover
  point in the caption, and has a FAT/CHO legend; `MFO_kinetics_plot` shows
  d, s, t, Fatmin and R² in the subtitle. Values are unchanged.

* `substrate_oxidation()` no longer fails on vectors with
  `author = "Jeukendrup"`; each value picks its own equation.

## Documentation

* New vignette "From gas exchange to Fatmax" (`vignette("mfo-workflow")`):
  each step of `MFO()` rebuilt by hand and checked against the package, the
  four methods compared, the SIN model and its sensitivity to
  `negative_FAT`, the effect of `RER_cutoff` and `include_basal`, a
  generated methods paragraph, and batch processing with `MFOs()`.

* pkgdown reference grouped by workflow; the vignette is the "Get started"
  article.

* README rewritten for v0.2.x and generated from an executable `README.Rmd`.

* Coefficients printed in the source are used as printed and cited by table
  or equation; derived coefficients are used only where the source prints no
  equation, with the derivation explained in the documentation (SPEC 19).
  Exercise equations of Jeukendrup & Wallis (2005, Table 5) and Frayn (1983)
  are used as printed; their derivation from the published O2/CO2 constants
  is kept as a cross-check test.

* Fixed the DOI of Achten et al. (2002) in `DESCRIPTION`.

* GitHub Actions: `R CMD check` on macOS, Windows and Ubuntu (R devel,
  release, oldrel-1), test coverage with Codecov, and a pkgdown site
  deployed to <https://jorgedelro.github.io/MFO/>.

* Tests for `MFOs()` in both modes (`"folder"` and `"files"`), checked
  against `MFO()` participant by participant; coverage of `R/MFOs.R` goes
  from 0 to 99 %.

## Known issues (planned for 0.3.0)

* `MFO_db$Load` is a synthetic step index (`step * step_duration / 60 * 5`),
  not measured load, and Fatmax is not returned in external-load units
  (W, km/h). A measured `Load` column is planned (SPEC 9.5, 14).
* The SIN model is normalised to the highest observed step, so a noisy
  step rescales the whole curve; normalising to the fitted MFO will be
  evaluated (SPEC 9.5).
* No confidence intervals for MFO, Fatmax or Fatmin yet (SPEC 13).

* The default of `energy_equivalents` will change from `"classic"` to
  `"source"`.

# MFO 0.2.0

GitHub release (February 2026). Not submitted to CRAN.

## New features

* `MFO()` gains `method` to choose the curve-fitting method: `"P3"`
  (third-degree polynomial, new default), `"P2"` and `"MRER"`. Version 0.1.0
  always used a second-degree polynomial.
* `MFO()` gains `author = "Peronnet"` (Péronnet & Massicotte, 1991) in
  addition to `"Frayn"` and `"Jeukendrup"`.
* New protocol arguments in `MFO()` and `MFOs()`: `step_duration` (default
  180 s) and `steady_state_duration` (default 60 s).
* New artefact handling: `RER_cutoff` removes steps above an RER threshold;
  `negative_FAT` chooses whether negative fat oxidation is truncated to 0,
  excluded or kept.
* New outputs in `MFO()`: Fatmax zone (fat oxidation within 10 % of MFO),
  crossover point, heart rate at Fatmax and Fatmax as %HRmax (`HRmax`), MFO
  relative to body mass (`body_mass`) and to fat-free mass (`fat_free_mass`),
  and `R_squared` / `RMSE` of the fit.
* `MFO_kinetics()` now returns standard errors of `d`, `s` and `t`,
  `FatMax_SIN`, `Fatmin`, `R_squared`, `RMSE` and a `convergence` flag, and
  handles fitting failures without stopping.
* `MFOs()` passes all new `MFO()` arguments through and gains
  `output_format` (`"xlsx"`, `"csv"` or `"rds"`).
* Internal helper to average breath-by-breath data into fixed intervals.

## Other changes

* Requires R >= 4.1.0 (was R >= 2.10).
* Expanded unit tests.

# MFO 0.1.0

First CRAN release (February 2022).

* `MFO()` computes MFO and Fatmax from resting, incremental and maximal test
  data, with Frayn (1983) or Jeukendrup & Wallis (2005) equations and a
  second-degree polynomial fit.
* `MFO_kinetics()` fits the SIN model of Cheneviere et al. (2009) and returns
  dilatation, symmetry and translation.
* `MFOs()` processes a folder of participants.
