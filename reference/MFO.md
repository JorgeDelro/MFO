# Maximal Fat Oxidation & FatMax Function

Calculate Maximal Fat Oxidation (MFO), the exercise intensity that
elicits MFO (FatMax), the FatMax zone, and the crossover point. Supports
multiple curve-fitting methods (P3, P2, SIN, MRER) and stoichiometric
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
  vo2max_window = 30
)
```

## Arguments

- step_time:

  how often the data was collected (in seconds).

- db_MFO:

  database containing MFO test.

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

- VO2max:

  VO2max can be passed directly (ml/min) instead of using db_graded. If
  both are given, \`VO2max\` is used as is and \`db_graded\` is ignored
  with a warning.

- method:

  curve-fitting method: "P3" (3rd-degree polynomial, default), "P2"
  (2nd-degree polynomial), or "MRER" (RER-based method of Perez-Martin
  et al.).

- step_duration:

  duration of each exercise step in seconds (default 180 = 3 min).

- steady_state_duration:

  duration of steady-state period to average within each step, in
  seconds (default 60).

- RER_cutoff:

  RER value above which steps are excluded (default NULL = no cutoff).
  Recommended values: 1.0 (general), 0.93 (sedentary/obese), 0.90
  (trained).

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

## Value

Returns a list which contains:

- MFO_db: database with Load, VO2, VCO2, HR, RER, CHO, FAT, Kcal,
  porc_VO2.

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

- crossover_point: Intensity at which CHO \> FAT (% VO2max).

- R_squared: R-squared of the fitted model.

- RMSE: Root mean squared error of the fitted model.

- method: Method used for curve fitting.

- VO2max: VO2max used to express intensity (ml/min).

- VO2max_window: effective averaging window for VO2max (s); NA if VO2max
  was supplied.

- VO2max_window_requested: \`vo2max_window\` as requested (s); NA if
  VO2max was supplied.

- x_CHO: carbohydrates in basal metabolism (g/min), with the same
  \`author\` equations as the test.

- x_FAT: fat in basal metabolism (g/min).

- x_Kcal: Kcal in basal metabolism (kcal/min).

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
