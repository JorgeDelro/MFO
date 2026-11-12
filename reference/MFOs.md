# Maximal Fat Oxidation calculation of multiple databases

Process multiple participants' MFO data in batch, generating plots and
summary results. Supports all parameters from the MFO() and
MFO_kinetics() functions.

## Usage

``` r
MFOs(
  from = c("folder", "files"),
  path,
  db_basal_name,
  db_MFO_name,
  db_graded_name,
  step_time,
  cv_var,
  author,
  method = "P3",
  VO2max = NULL,
  step_duration = 180,
  steady_state_duration = 60,
  RER_cutoff = NULL,
  negative_FAT = "truncate",
  body_mass = NULL,
  fat_free_mass = NULL,
  HRmax = NULL,
  remove_rows = NULL,
  col_name_VO2 = "VO2",
  col_name_VCO2 = "VCO2",
  col_name_RER = "RER",
  col_name_HR = "HR",
  save_plot = TRUE,
  save_result = TRUE,
  output_format = "xlsx",
  vo2max_window = 30,
  include_basal = FALSE,
  fatmin_threshold = NULL,
  energy_equivalents = c("classic", "source"),
  use_incomplete_step = FALSE,
  min_steps = 6
)
```

## Arguments

- from:

  select "folder" (basal, MFO and graded databases of the same
  participant are stored in different files but in the same folder) or
  "files" (basal, MFO and graded databases of the same participant are
  stored in one file but in different sheets)

- path:

  path to the folder with the databases

- db_basal_name:

  name given to the basal database, e.g. "basal_df"

- db_MFO_name:

  name given to the MFO database, e.g. "MFO_df"

- db_graded_name:

  name given to the graded database, e.g. "VO2max_df"

- step_time:

  how often the data was collected (in seconds).

- cv_var:

  variable to estimate coefficient of variation. Can be: "VO2", "VCO2"
  or "RER".

- author:

  equation set: "Frayn", "Jeukendrup" or "Peronnet".

- method:

  method to locate MFO: "P3" (default), "P2", "MV" or "MRER". See
  \[MFO()\].

- VO2max:

  VO2max can be passed directly (ml/min). Default NULL. If given, it is
  used for every participant and the graded databases are ignored.

- step_duration:

  duration of each exercise step in seconds (default 180).

- steady_state_duration:

  duration of steady-state period within each step in seconds (default
  60).

- RER_cutoff:

  RER value above which steps are excluded (default NULL), as a number
  or "classic", "safe", "sedentary", "trained". See \[MFO()\].

- negative_FAT:

  how to handle negative fat oxidation: "truncate", "exclude", or
  "keep".

- body_mass:

  body mass in kg (optional).

- fat_free_mass:

  fat-free mass in kg (optional).

- HRmax:

  maximum heart rate in bpm (optional).

- remove_rows:

  rows to exclude from analysis.

- col_name_VO2:

  column name for VO2 in databases. Default "VO2".

- col_name_VCO2:

  column name for VCO2 in databases. Default "VCO2".

- col_name_RER:

  column name for RER in databases. Default "RER".

- col_name_HR:

  column name for HR in databases. Default "HR".

- save_plot:

  save plots to MFO_plots/ directory (default TRUE).

- save_result:

  save results file (default TRUE).

- output_format:

  format for results file: "xlsx", "csv", or "rds". Default "xlsx".

- vo2max_window:

  averaging window in seconds for VO2max from the graded database
  (default 30). See \[MFO()\].

- include_basal:

  logical; include the resting row as step 0 in the fit (default
  \`FALSE\`). See \[MFO()\].

- fatmin_threshold:

  \`NULL\` (default, Fatmin at the second zero of the SIN) or a
  proportion of MFO in (0, 1). See \[MFO_kinetics()\].

- energy_equivalents:

  "classic" (default in 0.2.x) or "source". See \[MFO()\].

- use_incomplete_step:

  logical; use an incomplete last step that holds the steady-state
  segment (default \`FALSE\`). See \[MFO()\].

- min_steps:

  minimum number of exercise steps recommended for the fit (default 6);
  fewer give a warning. See \[MFO()\].

## Value

Data frame with one row per participant containing:

- ID: Participant identifier.

- MFO_g_min: Maximal fat oxidation (g/min).

- MFO_relative: MFO relative to body mass (mg/kg/min), if body_mass
  provided.

- MFO_FFM: MFO relative to fat-free mass (mg/kgFFM/min), if
  fat_free_mass provided.

- VO2max: VO2max used (ml/min).

- VO2max_window: effective averaging window for VO2max (s); NA if VO2max
  was supplied.

- FAT_MAX_perVO2max: FatMax as % VO2max.

- FAT_MAX_HR: Heart rate at FatMax (bpm).

- FAT_MAX_porcHRmax: FatMax as % HRmax, if HRmax provided.

- FatMax_zone_lower: Lower bound of FatMax zone (% VO2max).

- FatMax_zone_upper: Upper bound of FatMax zone (% VO2max).

- FatMax_zone_method: "fitted curve", or "linear interpolation" for
  "MV".

- crossover_point: Crossover intensity (% VO2max).

- R_squared: R-squared of the curve fit.

- method: method requested.

- energy_equivalents: "classic" or "source".

- d: SIN dilatation parameter.

- t: SIN translation parameter.

- s: SIN symmetry parameter.

- Fatmin: Fatmin from SIN model (% VO2max).

- FatMax_SIN: FatMax from SIN model (% VO2max).

- SIN_R_squared: R-squared of SIN model.

- SIN_convergence: whether the SIN model was fitted.

- CHO_g_basal: Basal CHO oxidation (g/min).

- FAT_g_basal: Basal FAT oxidation (g/min).

- Kcal_g_basal: Basal energy expenditure (kcal/min).

## Details

Every warning raised for a participant is re-issued with the participant
ID first (e.g. \`"P02: No HR data in db_MFO: ..."\`), so the quality
warnings of \[MFO()\] (missing HR, few steps, Fatmax at the edge,
incomplete last step) can be traced in a batch. A participant whose data
cannot be processed gives a row of \`NA\` and a warning with the error.

## Examples

``` r

if (FALSE) { # \dontrun{
# MFOs function
results <- MFOs(from = "folder",
                path = "/path/to/participant_data",
                db_basal_name = "basal_df",
                db_MFO_name = "MFO_df",
                db_graded_name = "graded_df",
                step_time = 20,
                cv_var = "RER",
                author = "Frayn",
                method = "P3")
} # }
```
