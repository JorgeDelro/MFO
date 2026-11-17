# VO2max of a graded test, with maximality criteria

Computes VO2max as the highest mean VO2 over \`window\` seconds of
consecutive records (as \`MFO()\` does from \`db_graded\`) and checks
whether the test was maximal, following the criteria reviewed by Howley,
Bassett and Welch (1995, Med Sci Sports Exerc 27(9): 1292-1301,
[doi:10.1249/00005768-199509000-00009](https://doi.org/10.1249/00005768-199509000-00009)
). The value is labelled \`"VO2max"\` when VO2 reached a plateau, or,
without a plateau, when both the RER and the heart-rate criteria are
met; otherwise it is labelled \`"VO2peak"\` (SPEC 14).

## Usage

``` r
VO2max_from_test(
  db_graded,
  step_time,
  window = 30,
  age = NULL,
  HRmax_formula = c("tanaka", "fox"),
  plateau_threshold = 150,
  RER_threshold = 1.1,
  HR_threshold = 0.9
)
```

## Arguments

- db_graded:

  data frame of the graded test with \`VO2\` (ml/min) and, for the
  criteria, \`RER\`, \`HR\` (bpm) and \`Load\`.

- step_time:

  how often the data were collected (s).

- window:

  averaging window in seconds (default 30). If it is not a multiple of
  \`step_time\`, the number of records is rounded up and a message
  reports the effective window (as \`vo2max_window\` in \[MFO()\]).

- age:

  age in years, for the predicted HRmax (optional).

- HRmax_formula:

  \`"tanaka"\` (default) or \`"fox"\`.

- plateau_threshold:

  VO2 increase between the last two stages below which there is a
  plateau, ml/min (default 150).

- RER_threshold:

  RER above which the RER criterion is met (default 1.10).

- HR_threshold:

  proportion of the predicted HRmax that the peak HR must reach (default
  0.90).

## Value

An object of class \`MFO_VO2max\`: a list with \`VO2max\` (ml/min),
\`type\` (\`"VO2max"\` or \`"VO2peak"\`), \`window\` (effective, s),
\`window_requested\`, \`n_records\`, \`criteria\` (a data frame with one
row per criterion: \`criterion\`, \`value\`, \`threshold\`, \`met\`,
\`NA\` when not evaluated), \`HRmax_predicted\`, \`HRmax_formula\` and
\`age\`. It can be passed to \[MFO()\] as \`VO2max\`.

## Details

Criteria (thresholds are arguments):

- plateau: the mean VO2 of the final stage is less than
  \`plateau_threshold\` ml/min above that of the previous stage (150
  ml/min, from Taylor et al. 1955 as reported by Howley et al. 1995).
  Stages are runs of equal \`Load\` above 0, each averaged over its last
  \`window\` s; the final stage is the last one at the highest load, so
  rest pauses (load 0) and a recovery after it are ignored. Without a
  \`Load\` column the criterion is not evaluated.

- RER: the highest \`window\`-s mean RER is above \`RER_threshold\`.

- HR: the highest \`window\`-s mean HR is at least \`HR_threshold\` of
  the predicted HRmax. Needs \`age\`; without it the criterion is not
  evaluated and a warning says so.

Predicted HRmax: \`"tanaka"\` 208 - 0.7 x age (default; Tanaka, Monahan
and Seals 2001, J Am Coll Cardiol 37(1): 153-156,
[doi:10.1016/s0735-1097(00)01054-8](https://doi.org/10.1016/s0735-1097%2800%2901054-8)
) or \`"fox"\` 220 - age, the equation in common use that Tanaka et al.
(2001) revisit. The RER (1.10) and HR (90 %) thresholds are pending
source verification (SPEC 19).

## See also

\[MFO()\]

## Examples

``` r
data(VO2max_df, package = "MFO")
VO2max_from_test(VO2max_df, step_time = 20, age = 30)
#> VO2max: a 30-s window is not a multiple of step_time (20 s); using 2 record(s) = 40 s.
#> <MFO_VO2max> VO2max = 3315.0 ml/min (highest 40-s mean)
#>   plateau  not met        value 247, threshold 150
#>   RER      met            value 1.16, threshold 1.1
#>   HR       met            value 0.987, threshold 0.9
```
