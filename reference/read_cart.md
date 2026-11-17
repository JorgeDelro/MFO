# Read a metabolic cart export

Reads the breath-by-breath export of a metabolic cart, averages it into
fixed intervals and returns the tables that \[MFO()\] expects, in the
package's internal data contract (SPEC 16). This is the only layer that
knows the proprietary formats.

## Usage

``` r
read_cart(
  file,
  device = NULL,
  interval = 10,
  cuts = NULL,
  phase = c("MFO", "basal", "graded"),
  body_mass = NULL,
  vo2_unit = c("auto", "mL/min", "L/min", "mL/kg/min")
)
```

## Arguments

- file:

  path of the export.

- device:

  \`NULL\` (detected by spiro), \`"cortex"\`, \`"cosmed"\`, \`"vyntus"\`
  or \`"zan"\`.

- interval:

  length of the averaging intervals in seconds (default 10); it becomes
  \`step_time\` for \[MFO()\].

- cuts:

  optional named vector of cut times in seconds, with \`basal_end\`
  and/or \`MFO_end\`.

- phase:

  without \`cuts\`, the phase of the whole recording: \`"MFO"\`
  (default), \`"basal"\` or \`"graded"\`.

- body_mass:

  body mass in kg, needed only when \`vo2_unit = "mL/kg/min"\`.

- vo2_unit:

  unit of VO2 and VCO2 in the export: \`"auto"\` (default),
  \`"mL/min"\`, \`"L/min"\` or \`"mL/kg/min"\`.

## Value

An object of class \`MFO_cart\`: a list with \`db_basal\`, \`db_MFO\`,
\`db_graded\` (data frames with \`Time\`, \`VO2\`, \`VCO2\`, \`RER\`
and, when the export has them, \`HR\`, \`Load\`, \`VE\`; \`NULL\` for a
phase not in the file), \`step_time\` (= \`interval\`) and \`info\`
(device, interval, VO2 unit, cuts, number of breaths, of intervals and
of empty intervals, and the RER mismatch count, \`NA\` when the export
has no RER).

## Details

The file is parsed with \`spiro::spiro_raw()\` (package spiro, rOpenSci,
JOSS 2023; in Suggests), which reads Cortex (MetaSoft Studio), Cosmed
(Omnia, Quark CPET, K5), Vyaire Vyntus CPX (SentrySuite) and ZAN
exports. MFO has validated none of these devices with a real export of
its own yet (one anonymised file per device is planned in
\`inst/extdata\`); ZAN is tested only with the example files of spiro.

\*\*Averaging.\*\* The breaths are averaged in consecutive intervals of
\`interval\` s from time 0: each value is the mean of the breaths
measured in that interval, not an interpolation. An interval without
breaths is kept as a row of \`NA\`, so the records stay aligned in time
(\`MFO()\` cuts steps by counting records). The default of 10 s divides
the usual step durations (60-300 s) and the 30-s windows of VO2max and
ramps exactly, so they need no rounding.

\*\*Units.\*\* VO2 and VCO2 are returned in mL/min. With \`vo2_unit =
"auto"\` a median VO2 above 100 is read as mL/min; at or below 100 the
unit must be given, because L/min and mL/kg/min cannot be told apart
reliably. RER is always VCO2/VO2 of the interval means; when the export
has its own RER, a warning counts the intervals where they differ by
more than 0.02. Load is passed through unconverted: declare its unit in
\[MFO_protocol()\].

\*\*Phases.\*\* \`cuts\` gives the times (s from the start) at which the
resting measure ends (\`basal_end\`) and the MFO test ends
(\`MFO_end\`). With both, the recording is split into basal, MFO and
graded tables; with \`MFO_end\` only, into MFO and graded (a single-day
test, see \[MFO_protocol()\]); with \`basal_end\` only, into basal and
MFO. Without \`cuts\`, the whole recording is the phase named in
\`phase\`. Detecting the phases from the analyser's markers will come
with each device importer.

No personal data from the file are kept: the result holds only the
measured variables and technical information.

## See also

\[MFO()\], \[MFO_protocol()\]

## Examples

``` r
# \donttest{
if (requireNamespace("spiro", quietly = TRUE)) {
  # A graded test exported by a ZAN cart (example file of spiro)
  cart <- read_cart(spiro::spiro_example("zan_gxt"), phase = "graded")
  cart
  VO2max_from_test(cart$db_graded, step_time = cart$step_time)
}
#> Warning: age not given: the heart-rate criterion of VO2max is not evaluated.
#> <MFO_VO2max> VO2peak = 4727.2 ml/min (highest 30-s mean)
#>   plateau  not met        value 1655.019, threshold 150
#>   RER      not met        value 0.991, threshold 1.1
#>   HR       not evaluated  value NA, threshold 0.9
# }
```
