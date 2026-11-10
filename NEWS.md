# MFO (development version)

Target: v0.2.1 on CRAN. Scientific background and sources in `docs/SPEC.md`.

## Infrastructure

* GitHub Actions: `R CMD check` on macOS, Windows and Ubuntu (R devel,
  release, oldrel-1), test coverage with Codecov, and a pkgdown site
  deployed to <https://jorgedelro.github.io/MFO/>.
* README rewritten for v0.2.x and generated from an executable `README.Rmd`.
* Fixed the DOI of Achten et al. (2002) in `DESCRIPTION`.

## Bug fixes

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
  from their Table 4 (CHO = 4.585 VCO2 − 3.226 VO2, FAT = 1.695 VO2 −
  1.701 VCO2). MFO and Fatmax are not affected because the resting row is not
  part of the fit. Example data with `"Peronnet"`: resting CHO
  0.130 -> 0.134 g/min, FAT 0.0726 -> 0.0723 g/min (SPEC 9.5).
* Jeukendrup & Wallis (2005) coefficients are now derived in code from the
  O2/CO2 constants of their Table 4 (glucose/glycogen mix 50/50 for
  40–50 %VO2max, 20/80 for 50–75 %VO2max) instead of the rounded published
  values (differences ≤ 0.005). Example data with `"Jeukendrup"`: MFO 0.3163
  -> 0.3158 g/min, Fatmax 35.36 -> 35.29 %VO2max (SPEC 19).
* `substrate_oxidation()` no longer fails on vectors with
  `author = "Jeukendrup"`; each value picks its own equation.
* When both `VO2max` and `db_graded` are given, `VO2max` is now used and
  `db_graded` is ignored with a warning (v0.2.0 silently used `db_graded`).

## New features

* `MFO()` returns `VO2max`, `VO2max_window` (effective, s) and
  `VO2max_window_requested`; `MFOs()` gains `vo2max_window` and the columns
  `VO2max` and `VO2max_window`.

## Known issues (scheduled for this release)

Found while verifying the code against the primary sources (SPEC sections
9.5 and 19):

* The SIN model in `MFO_kinetics()` computes `pi^(1/s) / pi + 2*d` instead of
  `pi^(1/s) / (pi + 2*d)` (Cheneviere et al., 2009, eq. 14). Values of `d`
  from v0.1.0/v0.2.0 are not comparable with published values.
* `MFO_kinetics()` fits the SIN model to a P3-smoothed synthetic grid instead
  of the observed steps, and defines `Fatmin` with a 5 % threshold instead of
  the second zero of the curve.
* `method = "MRER"` is a measured-values method on `1.7 * (1 - RER) * VO2`,
  not the two-regression MRER procedure.
* The resting row is dropped before the curve fit (`include_basal` option
  planned); `MFO_db$Load` is a synthetic step index, not measured load.
* Péronnet–Massicotte coefficients are rounded constants rather than derived
  from their Table 1; energy equivalents are 4/9 kcal/g for all equations
  (Jeukendrup & Wallis 2005, Table 5, give CHO 3.95 kcal/g at 40–50 %VO2max,
  4.07 kcal/g at 50–75 % and fat 9.75 kcal/g).

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
