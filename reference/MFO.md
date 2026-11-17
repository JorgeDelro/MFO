# Maximal Fat Oxidation & FatMax Function

Calculate Maximal Fat Oxidation (MFO), the exercise intensity that
elicits MFO (FatMax), the FatMax zone, and the crossover point. Supports
multiple methods to locate MFO (P3, P2, MV, MRER) and stoichiometric
equations (Frayn, Jeukendrup, Peronnet).

## Usage

``` r
MFO(
  step_time,
  db_MFO,
  db_basal,
  db_graded = NULL,
  cv_var,
  author,
  VO2max = NULL,
  method = "P3",
  step_duration = 180,
  steady_state_duration = 60,
  RER_cutoff = NULL,
  negative_FAT = "truncate",
  body_mass = NULL,
  fat_free_mass = NULL,
  HRmax = NULL,
  vo2max_window = 30,
  include_basal = FALSE,
  energy_equivalents = c("classic", "source"),
  use_incomplete_step = FALSE,
  min_steps = 6,
  protocol = NULL,
  age = NULL,
  ci = c("none", "parametric", "bootstrap"),
  B = 2000,
  conf = 0.95,
  seed = NULL,
  show_ci = TRUE
)
```

## Arguments

- step_time:

  how often the data was collected (in seconds).

- db_MFO:

  database containing MFO test. An optional \`Load\` column (W or km/h,
  in the unit of \`protocol\`) is averaged per step like VO2 and gives
  Fatmax and the Fatmax zone in load units.

- db_basal:

  database containing basal test.

- db_graded:

  database containing incremental exercise test (optional if VO2max
  provided). VO2max is the highest mean VO2 over \`vo2max_window\`
  seconds of consecutive records.

- cv_var:

  variable to estimate coefficient of variation. Can be: "VO2", "VCO2"
  or "RER".

- author:

  author to estimate MFO. Can be: "Frayn", "Jeukendrup" or "Peronnet".
  "Jeukendrup" warns when an exercise step is outside 40-75 % VO2max,
  the range of Jeukendrup & Wallis (2005, Table 5); the resting row
  never warns.

- VO2max:

  VO2max can be passed directly (ml/min) instead of using db_graded, or
  as the result of \[VO2max_from_test()\] (to set its thresholds or
  HRmax equation). If both are given, \`VO2max\` is used as is and
  \`db_graded\` is ignored with a warning.

- method:

  method to locate MFO and Fatmax: "P3" (3rd-degree polynomial,
  default), "P2" (2nd-degree polynomial), "MV" (measured values) or
  "MRER" (RER method). See the section \*Methods\*.

- step_duration:

  duration of each exercise step in seconds (default 180 = 3 min).

- steady_state_duration:

  duration of steady-state period to average within each step, in
  seconds (default 60).

- RER_cutoff:

  RER value above which steps are excluded (default NULL = no cutoff),
  as a number or by name, following Amaro-Gahete et al. (2019, Front
  Physiol 10: 909,
  [doi:10.3389/fphys.2019.00909](https://doi.org/10.3389/fphys.2019.00909)
  ): "classic" = 1.00 (Cheneviere et al. 2009 averaged only steps with
  RER \<= 1.0); "safe" = 0.95, the end-of-test RER they recommend as
  safe to capture MFO; "sedentary" = 0.93 and "trained" = 0.90, the mean
  RER at MFO they report in those groups. Low cutoffs leave few steps
  (SPEC 4, 19 finding 2).

- negative_FAT:

  how to handle negative fat oxidation values: "truncate" (set to 0,
  default), "exclude" (remove from fitting), or "keep" (include in
  fitting).

- body_mass:

  body mass in kg (optional, for relative MFO calculation).

- fat_free_mass:

  fat-free mass in kg (optional, for FFM-relative MFO calculation).

- HRmax:

  maximum heart rate in bpm (optional, for FatMax as %HRmax).

- vo2max_window:

  averaging window in seconds for VO2max from \`db_graded\` (default
  30). If it is not a multiple of \`step_time\`, the number of records
  is rounded up (30 s with 20-s records uses 2 records = 40 s). See SPEC
  9.5 and 14.

- include_basal:

  logical. If \`TRUE\`, the resting row (from \`db_basal\`) enters
  \`MFO_db\` as step 0 and is part of the curve fit, as in Montes de Oca
  et al. (2021, Eur J Sport Sci,
  [doi:10.1080/17461391.2020.1788650](https://doi.org/10.1080/17461391.2020.1788650)
  ). Default \`FALSE\` fits exercise steps only and reproduces
  v0.1.0/v0.2.0, which dropped the resting row. With \`TRUE\` the P3/P2
  fit changes; the resting row has \`HR = NA\` and \`Load = "basal"\`.
  See SPEC 9.5.

- energy_equivalents:

  kcal per g used for \`Kcal\` and \`x_Kcal\`: "classic" (4 kcal/g CHO,
  9 kcal/g FAT, as in v0.2.0) or "source" (values of the source of
  \`author\`: Peronnet & Massicotte 1991 for "Peronnet" and, as a
  package decision, "Frayn"; Jeukendrup & Wallis 2005, Table 5, for
  "Jeukendrup"). Default "classic" in 0.2.x; it will change to "source"
  in 0.3.0, and a message says so once per session when the argument is
  not set. MFO and Fatmax (g/min) do not depend on it. See
  \[substrate_oxidation()\] and SPEC 19 (finding 1).

- use_incomplete_step:

  logical. Records left after the last complete step (fewer than
  \`step_duration\` s) always give a warning stating "X s of Y s". With
  \`FALSE\` (default, as in v0.2.0) they are dropped. With \`TRUE\` that
  incomplete step is used only if it lasts at least
  \`steady_state_duration\`; its last \`steady_state_duration\` s are
  averaged, as for complete steps, although VO2 may not have reached its
  plateau. See SPEC 9.5.

- min_steps:

  minimum number of exercise steps recommended for the fit (default 6).
  A warning is given when fewer steps remain after \`RER_cutoff\` and
  \`negative_FAT = "exclude"\`; the resting row of \`include_basal =
  TRUE\` does not count. See SPEC 14.

- protocol:

  optional \[MFO_protocol()\] describing the test: step durations (equal
  or variable), averaged segment, load and mode. If given,
  \`step_duration\` and \`steady_state_duration\` are not used (a
  warning says so if they were also supplied). Default \`NULL\` builds
  \`MFO_protocol("step", step_duration, steady_state_duration)\`, which
  gives the same results as v0.2.1. See SPEC 14.

- age:

  age in years (optional), for the heart-rate criterion of
  \[VO2max_from_test()\] when VO2max comes from \`db_graded\`. Without
  it that criterion is not evaluated and a message says so once per
  session.

- ci:

  \`"none"\` (default), \`"parametric"\` or \`"bootstrap"\`: percentile
  intervals of the fit for MFO, Fatmax, the Fatmax zone and Fatmax in
  load units (see \*Fit uncertainty\*). \`TRUE\` means \`"parametric"\`,
  the recommended one. Not available for \`method = "MV"\`. Their
  coverage by method, number of steps and noise is in
  \`vignette("validation")\`.

- B:

  number of bootstrap replicates (default 2000).

- conf:

  confidence level of the intervals (default 0.95).

- seed:

  optional integer for reproducible intervals; the random state of the
  session is restored afterwards.

- show_ci:

  logical; with \`ci = "bootstrap"\`, show the interval in the plot
  subtitle and Fatmax's as a grey band (default \`TRUE\`).

## Value

Returns a list which contains:

- MFO_db: database with Step, Load (see \*Load\*), VO2, VCO2, HR, RER,
  CHO, FAT, Kcal, porc_VO2 (one row per exercise step, plus the resting
  row first if \`include_basal = TRUE\`). \`Step\` is the step number in
  the test (0 = resting row); steps removed by \`RER_cutoff\` or
  \`negative_FAT = "exclude"\` leave gaps in it.

- MFO_plot: ggplot object with the MFO plot including FAT and CHO
  curves.

- MFO: Maximal fat oxidation (g/min).

- MFO_relative: MFO relative to body mass (mg/kg/min), if body_mass
  provided.

- MFO_FFM: MFO relative to fat-free mass (mg/kgFFM/min), if
  fat_free_mass provided.

- FAT_MAX: Intensity that elicits MFO (% VO2max).

- FAT_MAX_HR: Heart rate at FatMax (bpm), interpolated from data.

- FAT_MAX_porcHRmax: FatMax as % HRmax, if HRmax provided.

- FatMax_zone_lower: Lower bound of FatMax zone (% VO2max).

- FatMax_zone_upper: Upper bound of FatMax zone (% VO2max).

- FatMax_zone_method: "fitted curve", or "linear interpolation" for
  \`method = "MV"\`.

- FAT_MAX_load, FatMax_zone_lower_load, FatMax_zone_upper_load: Fatmax
  and the Fatmax zone in \`load_unit\` (\`NA\` without load).

- load_unit: "W", "km/h" or "W/kg" (\`NA\` without load).

- load_source: "file", "protocol" or "none".

- crossover_point: Intensity at which CHO \> FAT (% VO2max).

- R_squared: R-squared of the fitted model (\`NA\` for "MV").

- RMSE: Root mean squared error of the fitted model (\`NA\` for "MV").

- method: Method used for curve fitting.

- energy_equivalents: energy equivalents used for \`Kcal\` ("classic" or
  "source").

- VO2max: VO2max used to express intensity (ml/min).

- VO2max_window: effective averaging window for VO2max (s); NA if VO2max
  was supplied.

- VO2max_window_requested: \`vo2max_window\` as requested (s); NA if
  VO2max was supplied.

- VO2max_type: "VO2max" or "VO2peak" from the maximality criteria of
  \[VO2max_from_test()\]; NA if a number was supplied as VO2max.

- VO2max_criteria: data frame of those criteria (NULL if a number was
  supplied).

- x_CHO: carbohydrates in basal metabolism (g/min), with the same
  \`author\` equations as the test.

- x_FAT: fat in basal metabolism (g/min).

- x_Kcal: Kcal in basal metabolism (kcal/min).

- ci: with \`ci = "bootstrap"\`, a data frame with rows MFO, FAT_MAX,
  FatMax_zone_lower, FatMax_zone_upper and FAT_MAX_load and columns
  estimate, lower and upper; otherwise \`NULL\`.

- bootstrap: with \`ci = "bootstrap"\`, a list with \`B\`, \`kept\`
  (replicates used), \`conf\` and the \`replicates\` matrix; otherwise
  \`NULL\`.

- protocol: the \[MFO_protocol()\] used.

- mode: exercise mode of the protocol ("cycling", "running" or
  "walking").

## Fit uncertainty

The curve is refitted to B replicates made of the fitted values plus new
errors (SPEC 13.1), and the interval is the percentile one.

- \`ci = "parametric"\` (also \`ci = TRUE\`): for each replicate the
  error variance is drawn from its posterior, sigma^2 ~ Inv-chi^2(n - p,
  s^2) with s^2 = RSS / (n - p), and the errors are normal with that
  sigma. This is the posterior interval of normal regression with a flat
  prior (Gelman et al. 2013, Bayesian Data Analysis, 3rd ed., ch. 14),
  i.e. a parametric bootstrap that carries the uncertainty of sigma.

- \`ci = "bootstrap"\`: residuals resampled with replacement, modified,
  r / sqrt(1 - h), with h the leverage of each step (diagonal of X
  (X'X)^-1 X'), and centred (Davison & Hinkley 1997, Bootstrap Methods
  and their Application, ch. 6).

For \`"MRER"\` its two regressions, 1 - RER and VO2 on % VO2max, are
handled separately. Replicates in which the fit falls back to another
method (e.g. P3 to P2) are dropped and counted.

With few steps the residual SD is estimated with few degrees of freedom,
and only \`"parametric"\` carries that uncertainty. In the ideal case
(P2 on a parabola, 8 steps, independent normal errors; 500 simulated
tests, B = 500) the 95 % intervals covered Fatmax 94.4 %
(\`"parametric"\`) and 88.2 % (\`"bootstrap"\`), and MFO 95.4 % and 89.2
%; the bootstrap behaves like a z interval with 5 degrees of freedom,
P(\|t_5\| \< 1.96) = 0.893.

\*\*MRER.\*\* Against SIN-shaped curves (the truth of the simulation
study), MRER overestimated Fatmax by 3-4 % VO2max and MFO by 3-7 %, and
its interval is centred on that bias, so it covered the true Fatmax in
only 48-74 % of tests. It is kept for comparability with the literature
and is not recommended as the main method. The study takes SIN curves as
the truth, which favours the models closest to a SIN (P3), see
\`vignette("validation")\`.

\*\*P2.\*\* When the vertex of the parabola falls outside the measured
intensities, Fatmax is an extrapolation: a warning says so and the rows
of \`ci\` are flagged.

This is the uncertainty of the fit within one test, not the day-to-day
error (see \[MFO_reliability()\]). It assumes exchangeable residuals;
fat oxidation errors grow with intensity (FAT is about 1.67 (VO2 -
VCO2), which amplifies the error of VCO2), so the interval is
approximate. A wild bootstrap (Wu 1986, Ann Stat 14(4): 1261-1295,
[doi:10.1214/aos/1176350142](https://doi.org/10.1214/aos/1176350142) )
would allow for it, but with 6-10 steps and two-point weights it can
produce only 2^n distinct samples, so it is not offered.

## Load

\`Load\` in \`MFO_db\` is the measured external load of each step (mean
over the averaged segment) when \`db_MFO\` has a \`Load\` column;
otherwise the load schedule of \`protocol\` (\`load_start + (Step - 1)
\* load_increment\`); otherwise \`NA\` (\`load_source\` says which).
Fatmax and the Fatmax zone are converted to load units by linear
interpolation of Load against % VO2max between steps, as HR at Fatmax.
With \`MFO_protocol("relative")\` a \`Load\` column is taken as W and
divided by \`body_mass\` to give W/kg; without \`body_mass\` it stays in
W with a warning. Up to v0.2.1 \`Load\` was a synthetic step label. See
SPEC 14.

## Quality warnings

Besides the warnings on inputs, \`MFO()\` warns when the result needs
checking (SPEC 9.5, 14):

- fewer than \`min_steps\` exercise steps enter the fit;

- Fatmax is at the edge: the exercise step nearest to Fatmax is the
  first or the last one (resting row excluded), so the maximum may lie
  outside the measured range (the test started too high or stopped too
  early);

- the last step is incomplete (see \`use_incomplete_step\`);

- HR is missing: \`FAT_MAX_HR\` and \`FAT_MAX_porcHRmax\` are \`NA\`, or
  interpolated from fewer steps. HR never removes steps from the fit.

## Methods

- \`"P3"\`, \`"P2"\`: least-squares polynomial of FAT on % VO2max;
  Fatmax is the maximum of the fitted curve within the measured range
  (SPEC 6.2, 6.3).

- \`"MV"\` (measured values): Fatmax is the step with the highest
  observed FAT, computed with the \`author\` equations; MFO is that FAT.
  No model is fitted, so \`R_squared\` and \`RMSE\` are \`NA\`; the
  curve drawn and the Fatmax zone use linear interpolation between steps
  (\`FatMax_zone_method = "linear interpolation"\`). SPEC 6.1.

- \`"MRER"\`: procedure of Cheneviere et al. (2009, p. 1617,
  [doi:10.1249/MSS.0b013e31819e2f91](https://doi.org/10.1249/MSS.0b013e31819e2f91)
  ), after Perez-Martin et al. (2001). (1 - RER) and VO2 are fitted as
  straight lines of % VO2max, 1 - RER = a1 + b1 x and VO2 = a2 + b2 x,
  so fat oxidation 1.7 (1 - RER) VO2 is a parabola whose vertex, x =
  -(a1 b2 + a2 b1) / (2 b1 b2), gives Fatmax, and its value MFO (g/min).
  This uses the 1.7 (1 - RER) VO2 simplification of Peronnet &
  Massicotte whatever \`author\` is. If the parabola has no maximum or
  the vertex lies outside the measured range, \`"MV"\` is used with a
  warning. SPEC 3, 6.1.

Up to v0.2.0, \`"MRER"\` returned the step with the highest observed 1.7
(1 - RER) VO2, which is a measured-values method: use \`"MV"\` for that
behaviour (same step; MFO from the \`author\` equation). SPEC 19,
finding 4.

## Examples

``` r

if (FALSE) { # \dontrun{
# Read dfs
data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")

# Convert to data.frame
basal_df <- data.frame(basal_df)
MFO_df <- data.frame(MFO_df)
VO2max_df <- data.frame(VO2max_df)

# Calculate MFO and Fatmax (P3 method)
result_MFO <- MFO(step_time = 20,
                 db_MFO = MFO_df,
                 db_basal = basal_df,
                 db_graded = VO2max_df,
                 cv_var = "RER",
                 author = "Frayn",
                 method = "P3")

# With all options
result_MFO <- MFO(step_time = 20,
                 db_MFO = MFO_df,
                 db_basal = basal_df,
                 db_graded = VO2max_df,
                 cv_var = "RER",
                 author = "Frayn",
                 method = "P3",
                 RER_cutoff = 1.0,
                 body_mass = 75,
                 fat_free_mass = 60,
                 HRmax = 190)
} # }
```
