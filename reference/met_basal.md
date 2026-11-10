# Basal metabolic rate

Calculate basal metabolism from rest test data using a rolling window to
identify the most metabolically stable 5-minute period.

## Usage

``` r
met_basal(step_time, db, cv_var, window_duration = 300)
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
