# Basal metabolic rate

Calculate basal metabolism from rest test data using a rolling window to
identify the most metabolically stable 5-minute period.

## Usage

``` r
met_basal(
  step_time,
  db,
  cv_var,
  window_duration = 300,
  author = "Frayn",
  energy_equivalents = "classic"
)
```

## Arguments

- step_time:

  how often the data was collected (in seconds).

- db:

  a data frame with columns VO2, VCO2, RER, HR.

- cv_var:

  variable to calculate coefficient of variation: "VO2", "VCO2" or
  "RER".

- window_duration:

  duration of the stable window in seconds (default 300 = 5 min).

- author:

  equation set used for resting substrate oxidation: "Frayn" (default),
  "Jeukendrup" or "Peronnet". \`MFO()\` passes its own \`author\`, so
  resting and exercise values use the same equations (SPEC 9.5).
  Jeukendrup & Wallis (2005) propose no resting equation; with
  "Jeukendrup" the package uses the 100 % glucose pair derived from
  their Table 4 (see \[substrate_oxidation()\], \`phase = "rest"\`).

- energy_equivalents:

  "classic" (default) or "source"; see \[substrate_oxidation()\].
