# Maximal Fat Oxidation calculation of multiple databases

Process multiple participants' MFO data in batch and return the summary
results. Supports all parameters from the MFO() and MFO_kinetics()
functions. Nothing is written to disk unless \`output_dir\` is given
(see \*Output files\*).

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
  min_steps = 6,
  protocol = NULL,
  age = NULL,
  col_name_Load = "Load",
  ci = c("none", "parametric", "bootstrap"),
  B = 2000,
  conf = 0.95,
  seed = NULL,
  sin_fit = c("normalised", "absolute"),
  sin_ci = FALSE,
  output_dir = NULL
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

  VO₂max can be passed directly (ml/min). Default NULL. If given, it is
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

  column name for VO₂ in databases. Default "VO2".

- col_name_VCO2:

  column name for VCO₂ in databases. Default "VCO2".

- col_name_RER:

  column name for RER in databases. Default "RER".

- col_name_HR:

  column name for HR in databases. Default "HR".

- save_plot:

  with \`output_dir\`, save the plots of each participant in
  \`output_dir/MFO_plots\` (default TRUE).

- save_result:

  with \`output_dir\`, save the results table as
  \`output_dir/MFO_result.\<output_format\>\` (default TRUE).

- output_format:

  format for results file: "xlsx", "csv", or "rds". Default "xlsx".

- vo2max_window:

  averaging window in seconds for VO₂max from the graded database
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

- protocol:

  optional \[MFO_protocol()\], the same for every participant; if given,
  \`step_duration\` and \`steady_state_duration\` are not used. See
  \[MFO()\].

- age:

  age in years (optional), the same for every participant, for the
  heart-rate criterion of VO₂max. See \[MFO()\] and
  \[VO2max_from_test()\].

- col_name_Load:

  column name for the external load (W or km/h) in the databases.
  Default "Load"; the column is optional. See \[MFO()\].

- ci, B, conf, seed:

  confidence intervals of the fit, passed to \[MFO_kinetics()\] (only
  with \`sin_ci = TRUE\`) and \[MFO()\]: \`ci\` is \`"none"\` (default),
  \`"parametric"\` (also \`TRUE\`) or \`"bootstrap"\`. The same \`seed\`
  is used for every participant.

- sin_fit:

  \`"normalised"\` (default) or \`"absolute"\` fit of the SIN model. See
  \[MFO_kinetics()\].

- sin_ci:

  logical; if \`TRUE\`, \`ci\` is also computed for the SIN model
  (default \`FALSE\`, as it doubles the run time).

- output_dir:

  folder where plots and results are written (created if needed).
  \`NULL\` (default): nothing is written, the results are only returned
  (see \*Output files\*).

## Value

Data frame with one row per participant containing:

- ID: Participant identifier.

- MFO_g_min: Maximal fat oxidation (g/min).

- MFO_relative: MFO relative to body mass (mg/kg/min), if body_mass
  provided.

- MFO_FFM: MFO relative to fat-free mass (mg/kgFFM/min), if
  fat_free_mass provided.

- VO2max: VO₂max used (ml/min).

- VO2max_window: effective averaging window for VO₂max (s); NA if VO₂max
  was supplied.

- VO2max_type: "VO2max" or "VO2peak" (see \[VO2max_from_test()\]); NA if
  VO₂max was supplied.

- FAT_MAX_perVO2max: Fatmax as % VO₂max.

- FAT_MAX_lower, FAT_MAX_upper, MFO_lower, MFO_upper: bootstrap
  confidence limits with \`ci = "bootstrap"\`, otherwise \`NA\`.

- FAT_MAX_HR: Heart rate at Fatmax (bpm).

- FAT_MAX_porcHRmax: Fatmax as % HRmax, if HRmax provided.

- FatMax_zone_lower: Lower bound of Fatmax zone (% VO₂max).

- FatMax_zone_upper: Upper bound of Fatmax zone (% VO₂max).

- FatMax_zone_method: "fitted curve", or "linear interpolation" for
  "MV".

- FatMax_zone_lower_HR, FatMax_zone_upper_HR: heart rate at the limits
  of the Fatmax zone (bpm).

- HRmax: \`HRmax\` as given (\`NA\` if not given).

- HR_peak: highest mean HR of the graded test over the VO₂max window
  (bpm); \`NA\` with a numeric \`VO2max\`. See \[MFO_zones()\].

- FAT_MAX_load, FatMax_zone_lower_load, FatMax_zone_upper_load: Fatmax
  and Fatmax zone in load units; load_unit and load_source as in
  \[MFO()\].

- crossover_point: Crossover intensity (% VO₂max).

- R_squared: R-squared of the curve fit.

- method: method requested.

- method_used: method actually used (differs after a fallback).

- energy_equivalents: "classic" or "source".

- author: stoichiometric equation used.

- protocol: protocol type (see \[MFO_protocol()\]).

- mode: exercise mode of the protocol.

- d: SIN dilatation parameter.

- t: SIN translation parameter.

- s: SIN symmetry parameter.

- Fatmin: Fatmin from SIN model (% VO₂max).

- FatMax_SIN: Fatmax from SIN model (% VO₂max).

- FatMax_SIN_lower, FatMax_SIN_upper, Fatmin_lower, Fatmin_upper:
  confidence limits of the SIN fit with \`ci\`, otherwise \`NA\`.

- MFO_SIN: MFO of the SIN curve with \`sin_fit = "absolute"\`, otherwise
  \`NA\`.

- sin_fit: SIN fit used.

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

## Output files

Up to v0.2.1 \`MFOs()\` wrote \`MFO_plots/\` and \`MFO_result.xlsx\` in
the working directory by default. It now writes only when \`output_dir\`
is given, as the CRAN Repository Policy asks of packages (no writing
outside the temporary directory unless the user asks for it). Without
\`output_dir\`, a message once per session says that nothing is written
and how to ask for it. \`save_plot\`, \`save_result\` and
\`output_format\` choose what is written in \`output_dir\`.

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
# The same, writing the plots and MFO_result.xlsx in a folder
results <- MFOs(from = "folder",
                path = "/path/to/participant_data",
                db_basal_name = "basal_df",
                db_MFO_name = "MFO_df",
                db_graded_name = "graded_df",
                step_time = 20,
                cv_var = "RER",
                author = "Frayn",
                output_dir = "MFO_output")
} # }
```
