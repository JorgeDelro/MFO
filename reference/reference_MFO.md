# Reference values for MFO and Fatmax

Published percentiles of maximal fat oxidation (MFO) and Fatmax in
sedentary adults: Table 1 of Amaro-Gahete, Sanchez-Delgado and Ruiz
(2018)
[doi:10.3389/fphys.2018.01460](https://doi.org/10.3389/fphys.2018.01460)
, 167 healthy sedentary adults (107 women) tested on a treadmill. One
row per group of the table: all participants, by sex, by age group, by
sex and age group, and by weight status. The percentiles are empirical
(P20, P40, P60, P80), not derived from the mean and SD. Every row
carries the test conditions, because a percentile is only meaningful
against a reference measured in the same way (SPEC 15.2).

The source does not print the cut-offs of its groups: age groups are the
young (22.1 +/- 2.2 years, n = 125) and middle-aged (52.1 +/- 4.6 years,
n = 42) cohorts, and weight status (normal weight, overweight, obese)
has no published BMI thresholds. Groups are therefore chosen explicitly,
never deduced from a numeric age or BMI.

Where the text and Table 1 disagree (Fatmax by weight status, SD of MFO
in middle-aged adults), the table is used.

## Usage

``` r
data(reference_MFO)
```

## Format

A data frame with 12 rows and 25 variables:

- cohort:

  source of the row, \`"Amaro-Gahete 2018"\`

- grouping:

  grouping criterion as printed in Table 1: \`"All"\`, \`"By Sex"\`,
  \`"By Age"\`, \`"By Sex and Age"\` or \`"By BMI Status"\`

- population:

  group label as printed in Table 1

- sex:

  \`"male"\`, \`"female"\` or \`NA\` (both)

- age_group:

  \`"young"\`, \`"middle-aged"\` or \`NA\` (both)

- weight_status:

  \`"normal"\`, \`"overweight"\`, \`"obese"\` or \`NA\` (all)

- n:

  number of participants in the group

- MFO_mean, MFO_sd:

  MFO mean and SD, in g/min

- MFO_p20, MFO_p40, MFO_p60, MFO_p80:

  MFO percentiles, in g/min

- Fatmax_mean, Fatmax_sd:

  Fatmax mean and SD, in % VO2max

- Fatmax_p20, Fatmax_p40, Fatmax_p60, Fatmax_p80:

  Fatmax percentiles, in % VO2max

- mode:

  exercise mode, \`"walking"\` (treadmill)

- protocol:

  graded protocol and averaging period

- equation:

  stoichiometric equation, Frayn (1983) without protein

- method:

  third-degree polynomial with intersection at (0;0) (Croci et al.
  2014), unlike the unconstrained \`"P3"\` of \[MFO()\]

- fasting:

  fasting before the test, 5-6 h (not overnight)

- doi:

  DOI of the source

## Source

Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:
Contextualising maximal fat oxidation during exercise: determinants and
normative values. \*Front Physiol\* 9:1460, Table 1.
[doi:10.3389/fphys.2018.01460](https://doi.org/10.3389/fphys.2018.01460)
. Built by \`data-raw/reference_MFO.R\`.

## Examples

``` r
data(reference_MFO, package = "MFO")
reference_MFO[, c("population", "n", "MFO_p20", "MFO_p80")]
#>                           population   n MFO_p20 MFO_p80
#> 1                   Sedentary adults 167    0.24    0.42
#> 2                Sedentary adult men  60    0.29    0.44
#> 3              Sedentary adult women 107    0.24    0.40
#> 4             Sedentary young adults 125    0.28    0.44
#> 5       Sedentary middle-aged adults  42    0.22    0.38
#> 6          Sedentary young adult men  41    0.28    0.48
#> 7        Sedentary young adult women  84    0.28    0.42
#> 8    Sedentary middle-aged adult men  19    0.29    0.42
#> 9  Sedentary middle-aged adult women  23    0.22    0.27
#> 10    Normal-weight sedentary adults  88    0.25    0.42
#> 11       Overweight sedentary adults  50    0.24    0.41
#> 12            Obese sedentary adults  29    0.26    0.45
```
