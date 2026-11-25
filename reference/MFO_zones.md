# Heart-rate zones and reference values for MFO and Fatmax

Expresses Fatmax and the Fatmax zone as heart rate (bpm and % of HRmax),
compares the zone with the recommendation for obesity of Chavez-Guevara
et al. (2023) when asked, and places MFO and Fatmax among the published
percentiles of \[reference_MFO\] (SPEC 15).

## Usage

``` r
MFO_zones(
  x,
  HRmax = NULL,
  predicted_HRmax = FALSE,
  age = NULL,
  HRmax_formula = c("tanaka", "fox"),
  population = c("general", "obesity"),
  body_fat = NULL,
  sex = NULL,
  age_group = NULL,
  weight_status = NULL
)
```

## Arguments

- x:

  the result of \[MFO()\] or the data frame returned by \[MFOs()\].

- HRmax:

  maximum heart rate in bpm (optional): one value, or one per row of
  \`x\`. It takes precedence over the \`HRmax\` and \`HR_peak\` of
  \`x\`.

- predicted_HRmax:

  logical; if \`TRUE\`, the predicted HRmax for \`age\` is used when no
  HRmax was given or measured (default \`FALSE\`).

- age:

  age in years, needed with \`predicted_HRmax = TRUE\`; one value or one
  per row of \`x\`, as \`body_fat\`, \`sex\`, \`age_group\` and
  \`weight_status\`.

- HRmax_formula:

  \`"tanaka"\` (default) or \`"fox"\`.

- population:

  \`"general"\` (default) or \`"obesity"\`, which adds the range of
  Chavez-Guevara et al. (2023).

- body_fat:

  body fat in % (needed with \`population = "obesity"\`).

- sex:

  \`"male"\` or \`"female"\` (optional) to choose the reference group.

- age_group:

  \`"young"\` or \`"middle-aged"\` (optional), the age groups of the
  reference.

- weight_status:

  \`"normal"\`, \`"overweight"\` or \`"obese"\` (optional); cannot be
  combined with \`sex\` or \`age_group\`.

## Value

A data frame of class \`MFO_zones\`, one row per result, with \`ID\`
(results of \[MFOs()\]), \`HRmax\`, \`HRmax_source\` (\`"given"\`,
\`"measured"\`, \`"predicted (Tanaka)"\`, \`"predicted (Fox)"\` or
\`NA\`), \`FAT_MAX\` and the zone (\`zone_lower\`, \`zone_upper\`; %
VO₂max), \`FAT_MAX_HR\`, \`zone_lower_HR\`, \`zone_upper_HR\` (bpm),
\`FAT_MAX_porcHRmax\`, \`zone_lower_porcHRmax\`,
\`zone_upper_porcHRmax\`, \`obesity_range_lower\`,
\`obesity_range_upper\` and \`obesity_zone\` (\`NA\` unless \`population
= "obesity"\`), \`MFO\`, \`MFO_percentile\`, \`MFO_percentile_label\`,
\`Fatmax_percentile\`, \`Fatmax_percentile_label\`, \`reference\`,
\`reference_n\` and \`reference_differences\`.

## HRmax

The HRmax used for the percentages is, in this order: \`HRmax\` given
here or in \[MFO()\]/\[MFOs()\]; the peak HR measured in the graded test
(\`HR_peak\`, the highest mean HR over the same window as VO₂max); and,
only when \`predicted_HRmax = TRUE\`, the predicted HRmax for \`age\`:
\`"tanaka"\` 208 - 0.7 x age (default; Tanaka, Monahan and Seals 2001,
[doi:10.1016/s0735-1097(00)01054-8](https://doi.org/10.1016/s0735-1097%2800%2901054-8)
) or \`"fox"\` 220 - age, the equation in common use described by
Tanaka, Monahan and Seals (2001). The column \`HRmax_source\` says which
one was used.

## Obesity

With \`population = "obesity"\`, the zone in % HRmax is compared with
the range of Chavez-Guevara et al. (2023,
[doi:10.1007/s40279-023-01897-y](https://doi.org/10.1007/s40279-023-01897-y)
): 61-66 % HRpeak with more than 35 % body fat and 57-64 % HRpeak with
less than 35 %; \`body_fat\` chooses the range. The source does not
cover exactly 35.0 %: there is no range (\`NA\`), with a message.
\`obesity_zone\` is \`"inside"\` (the whole zone falls in the range),
\`"partly overlaps"\` or \`"outside"\`. The range refers to the measured
HRpeak: with a predicted HRmax it is still shown, with a warning.

## Reference values

MFO (g/min) and Fatmax (% VO₂max) are placed among the empirical
percentiles P20, P40, P60 and P80 of Table 1 of Amaro-Gahete,
Sanchez-Delgado and Ruiz (2018,
[doi:10.3389/fphys.2018.01460](https://doi.org/10.3389/fphys.2018.01460)
), 167 sedentary adults, by linear interpolation. Below P20 or above P80
the label is \`"\< P20"\` or \`"\> P80"\` and the percentile \`NA\`:
there is no extrapolation. A percentile is never returned without its
group (\`reference\`, \`reference_n\`).

The group is chosen with \`sex\`, \`age_group\` and \`weight_status\`,
only in the combinations Table 1 prints: sex, age group, sex and age
group, or weight status. The source does not publish the age or BMI
cut-offs of its groups (young adults 22.1 +/- 2.2 years, middle-aged
adults 52.1 +/- 4.6 years), so the group is never deduced from a numeric
age or BMI.

The reference was measured walking on a treadmill. Results with \`mode =
"walking"\` or \`"running"\` get a percentile; with \`"cycling"\` the
percentile is \`NA\`, because the mode of exercise changes MFO and
Fatmax (Amaro-Gahete et al. 2018). \`reference_differences\` lists how
the test differs from the reference: mode, equation (Frayn without
protein in the reference), protocol (3-min steps averaged over the last
60 s), method (third-degree polynomial through the origin in the
reference, unlike the unconstrained \`"P3"\` of \[MFO()\]) and fasting
(5-6 h, not overnight).

## Warnings and messages

Every warning and message of this function is a classed condition:
\`mfo_warning\_\<id\>\` and \`mfo_warning\` (or \`mfo_message\_\<id\>\`
and \`mfo_message\`), with the numbers of the text as fields (for
example \`mfo_warning_negative_fat\` has \`n\`, the number of steps).
The text is English; the classes let a program, such as \[run_app()\],
handle a given warning or word it in another language:
\`withCallingHandlers(expr, mfo_warning_negative_fat = function(w)
w\$n)\`.

## References

Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:
Contextualising maximal fat oxidation during exercise: determinants and
normative values. Front Physiol 9: 1460, Table 1.
[doi:10.3389/fphys.2018.01460](https://doi.org/10.3389/fphys.2018.01460)

Chavez-Guevara IA, Amaro-Gahete FJ, Ramos-Jimenez A, Brun JF (2023).
Toward exercise guidelines for optimizing fat oxidation during exercise
in obesity: a systematic review and meta-regression. Sports Med 53(12):
2399-2416.
[doi:10.1007/s40279-023-01897-y](https://doi.org/10.1007/s40279-023-01897-y)

Tanaka H, Monahan KD, Seals DR (2001). Age-predicted maximal heart rate
revisited. J Am Coll Cardiol 37(1): 153-156.
[doi:10.1016/s0735-1097(00)01054-8](https://doi.org/10.1016/s0735-1097%2800%2901054-8)

## See also

\[MFO()\], \[MFOs()\], \[reference_MFO\]

## Examples

``` r
data(list = c("MFO_df", "basal_df", "VO2max_df"), package = "MFO")
res <- suppressWarnings(MFO(step_time = 20, db_MFO = MFO_df,
                            db_basal = basal_df, db_graded = VO2max_df,
                            cv_var = "RER", author = "Frayn"))
#> Kcal uses energy_equivalents = "classic" (4 kcal/g CHO, 9 kcal/g FAT). The default will change to "source" (values of the equation's source) in MFO 0.3.0. Set energy_equivalents explicitly to silence this message (shown once per session).
#> VO2max: a 30-s window is not a multiple of step_time (20 s); using 2 record(s) = 40 s.
#> VO2max_type: age not given, so the heart-rate criterion of VO2max is not evaluated and VO2max is labelled from the plateau only. Set age to evaluate it (shown once per session).
# MFO_df is a cycling test: zones in HR, but no percentile, because the
# reference was measured walking on a treadmill
MFO_zones(res, sex = "male", age_group = "young")
#> No percentile for 1 result(s) with mode "cycling": the reference (Amaro-Gahete et al. 2018) was measured walking on a treadmill, and the mode of exercise changes MFO and Fatmax. Set MFO_protocol(mode = ) if the test was walking or running.
#> MFO zones
#>   Fatmax:      36.2% VO2max, 128 bpm, 69.5% HRmax
#>   Fatmax zone: 24.2-48.3% VO2max, 107-145 bpm, 57.9-78.4% HRmax
#>   HRmax:       185 bpm (measured)
#>   Reference:   Amaro-Gahete 2018: Sedentary young adult men (n = 41)
#>     MFO 0.32 g/min: no percentile
#>     Fatmax 36.2 % VO2max: no percentile
#>     Differences: mode: cycling (reference: treadmill walking, no percentile); method: P3 (reference: third-degree polynomial through (0;0), Croci et al. 2014); fasting: reference 5-6 h, not overnight
# Range for obesity of Chavez-Guevara et al. (2023)
MFO_zones(res, population = "obesity", body_fat = 38)
#> No percentile for 1 result(s) with mode "cycling": the reference (Amaro-Gahete et al. 2018) was measured walking on a treadmill, and the mode of exercise changes MFO and Fatmax. Set MFO_protocol(mode = ) if the test was walking or running.
#> MFO zones
#>   Fatmax:      36.2% VO2max, 128 bpm, 69.5% HRmax
#>   Fatmax zone: 24.2-48.3% VO2max, 107-145 bpm, 57.9-78.4% HRmax
#>   HRmax:       185 bpm (measured)
#>   Obesity range (Chavez-Guevara et al. 2023): 61-66% HRpeak; zone partly overlaps
#>   Reference:   Amaro-Gahete 2018: Sedentary adults (n = 167)
#>     MFO 0.32 g/min: no percentile
#>     Fatmax 36.2 % VO2max: no percentile
#>     Differences: mode: cycling (reference: treadmill walking, no percentile); method: P3 (reference: third-degree polynomial through (0;0), Croci et al. 2014); fasting: reference 5-6 h, not overnight
```
