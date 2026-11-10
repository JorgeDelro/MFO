# MFO — R package for Maximal Fat Oxidation and kinetics

## What this repo is

R package `MFO` (CRAN: v0.1.0, 2022; GitHub: v0.2.0, Feb 2026). Computes
maximal fat oxidation (MFO), Fatmax, Fatmax zone, crossover point and
the SIN kinetics model (Chenevière 2009) from indirect-calorimetry data.
Target users: sport and health scientists. Roadmap: v0.2.x on CRAN -\>
v0.3.0 (reliability, uncertainty, protocols, importers, Shiny app) -\>
JOSS paper.

The scientific specification lives in `docs/SPEC.md` (exported from the
project doc “MFO: fundamentos científicos y técnicos”). Sections 9.5 and
12–17 are the backlog. Read SPEC.md before touching any formula. Never
change a coefficient without citing the source in the roxygen block and
adding a unit test against the paper’s numbers.

## Working rules

- Language: code, roxygen, NEWS, vignettes, commit messages in
  **English**. Conversation with Jorge in Spanish.
- Jorge decides product/domain questions (what the user sees, which
  default, which reference population). Claude decides architecture and
  explains each decision in one line.
- Be didactic: when introducing an R-package concept (roxygen tags,
  testthat edition 3, S3 classes, `Suggests` vs `Imports`, pkgdown,
  GitHub Actions) explain the *why* briefly.
- Small commits, one topic each; conventional prefixes (`feat:`, `fix:`,
  `docs:`, `test:`, `ci:`, `chore:`). Every scientific change references
  a SPEC.md section in the body.
- Never rewrite git history. Public history is a JOSS requirement.
- Backwards compatibility:
  [`MFO()`](https://jorgedelro.github.io/MFO/reference/MFO.md),
  [`MFO_kinetics()`](https://jorgedelro.github.io/MFO/reference/MFO_kinetics.md),
  [`MFOs()`](https://jorgedelro.github.io/MFO/reference/MFOs.md) keep
  their arguments. New behaviour enters via new arguments with defaults
  or new functions.
- Heavy optional deps (`brms`, `rstan`/`cmdstanr`, importer helpers) go
  in `Suggests` with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards.
  Core must install in seconds.

## Definition of done (every PR)

- `devtools::document()` run; `R CMD check --as-cran` 0 errors / 0
  warnings / 0 notes (document any unavoidable NOTE in
  `cran-comments.md`).
- `devtools::test()` green; new code covered; `covr` not lower than
  before.
- NEWS.md entry under the unreleased heading.
- If a formula or default changed: vignette and SPEC.md cross-reference
  updated.

## Phase 1 backlog (v0.2.1 -\> CRAN) — in order

1.  `ci:` GitHub Actions `R-CMD-check` (ubuntu/macos/windows) +
    `test-coverage` + `pkgdown`. Remove the dead Travis badge; add
    check/coverage/CRAN badges.
2.  `docs:` rewrite README for v0.2.x (methods P3/P2/MRER, equations,
    RER cutoff, relative outputs); fill NEWS.md (0.1.0, 0.2.0, 0.2.1).
3.  `fix:` VO2max from `db_graded` = highest rolling mean over
    `vo2max_window` s (default 30), not instantaneous max. SPEC 9.5 /
    14.
4.  `fix:` basal substrate oxidation uses the same `author` equation as
    the test. SPEC 9.5.
5.  `feat:` `include_basal = TRUE` argument (basal point as step 0 in
    the fit). SPEC 9.5.
6.  `fix:` Jeukendrup–Wallis equations warn outside 40–75 %VO2max. SPEC
    3 / 9.5.
7.  `fix:` SIN model formula: code has `(pi^(1/s) / pi + 2*d)`;
    Chenevière 2009 eq. \[14\] is `pi^(1/s) / (pi + 2*d)`. Add
    regression test with d = 0.18, s = 1.52, t = 0.21 (their Fig. 5):
    fitted curve must reproduce Fatmax = \[(pi+2d)\*2^(-1/s) - d - t\]/K
    and Fatmin = (pi+d-t)/K. SPEC 6.5 / 19 (finding 5). 7b. `fix:` SIN
    fitting procedure: drop the P3 smoothing and synthetic grid; fit
    nlsLM directly to the observed step values (%MFO, normalised to
    observed max). Fatmin = second zero of the curve (default),
    `fatmin_threshold` optional. SPEC 19 (findings 3, 7). 7c. `feat:`
    method `MV` (measured values, with the `author` equation) and a real
    `MRER` (linear fits of (1-RER) and VO2 vs intensity, vertex of their
    product). Current `MRER` behaviour becomes `MV`; keep `MRER` string
    working with a deprecation message for one release. SPEC 6.1 / 19
    (finding 4). 7d. `fix:` Rule: **printed coefficient in the source \>
    derived coefficient; derive only where the source prints no
    equation** (SPEC 19). Péronnet–Massicotte print no equations, so use
    coefficients derived in code from their Table 1 constants (O2 0.7455
    / CO2 0.7426 L per g glucose; 2.0092 / 1.4136 L per g fatty acid)
    with `.derive_oxidation_coefficients()`, the derivation explained in
    roxygen; unit test: 4.58519/3.22597 and 1.69469/1.70130. Frayn
    prints his equations, so keep 4.55/3.21 and 1.67/1.67 as printed;
    the derivation from his Table 1 is only a cross-check test. SPEC 19
    (finding 6). 7e. `feat:` `energy_equivalents` argument with
    per-equation defaults (Péronnet 3.8683/9.7460 kcal/g; Jeukendrup
    3.95 (40–50 %) / 4.07 (50–75 %) CHO and 9.75 fat, J&W 2005 Table 5;
    Frayn: product decision pending, see SPEC 19 finding 1).
    `RER_cutoff` accepts named values “classic” 1.00, “safe” 0.95,
    “sedentary” 0.93, “trained” 0.90 (Amaro-Gahete 2019). SPEC 19
    (findings 1, 2).
8.  `feat:` warn on incomplete last step and on missing HR; `min_steps`
    (default 6) warning; warning when Fatmax lands on the first or last
    step. SPEC 9.5 / 14.
9.  `docs:` vignette “From gas exchange to Fatmax” reproducing the
    README example and explaining every parameter. pkgdown site with
    reference grouped by workflow.
10. `chore:` `cran-comments.md`, `inst/CITATION`, submit v0.2.1 to CRAN.

Phase 2 (v0.3.0) starts after CRAN acceptance: SPEC sections 12–17, one
issue each.

## Repo conventions

- `R/` one file per exported function + `utils-*.R` for helpers;
  internal helpers prefixed `.`.
- Tests mirror `R/` file names. Numerical tests use tolerances stated in
  a comment with the source of the expected value.
- Example data: `data/` (rda) for small tables; `inst/extdata/` for raw
  analyzer exports.
- Datasets and example files must be anonymised; no participant
  identifiers ever.
