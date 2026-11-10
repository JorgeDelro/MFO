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
  vo2max_window = 30
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

  name given to the basal database, eg: "basal_df"

- db_MFO_name:

  name given to the MFO database, eg: "MFO_df"

- db_graded_name:

  name given to the graded database, eg: "VO2max_df"

- step_time:

  how often the data was collected (in seconds).

- cv_var:

  variable to estimate coefficient of variation. Can be: "VO2", "VCO2"
  or "RER".

- author:

  equation set: "Frayn", "Jeukendrup" or "Peronnet".

- method:

  curve-fitting method: "P3", "P2", or "MRER". Default "P3".

- VO2max:

  VO2max can be passed directly (ml/min). Default NULL. If given, it is
  used for every participant and the graded databases are ignored.

- step_duration:

  duration of each exercise step in seconds (default 180).

- steady_state_duration:

  duration of steady-state period within each step in seconds (default
  60).

- RER_cutoff:

  RER value above which steps are excluded (default NULL).

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

- crossover_point: Crossover intensity (% VO2max).

- R_squared: R-squared of the curve fit.

- d: SIN dilatation parameter.

- t: SIN translation parameter.

- s: SIN symmetry parameter.

- Fatmin: Fatmin from SIN model (% VO2max).

- FatMax_SIN: FatMax from SIN model (% VO2max).

- SIN_R_squared: R-squared of SIN model.

- CHO_g_basal: Basal CHO oxidation (g/min).

- FAT_g_basal: Basal FAT oxidation (g/min).

- Kcal_g_basal: Basal energy expenditure (kcal/min).

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
