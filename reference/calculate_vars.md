# Calculate step means from raw data

Generalized function that works with any step_time and step_duration.
Extracts the last minute of each step (steady-state) for averaging.

## Usage

``` r
calculate_vars(
  step_time,
  db_MFO,
  VO2max,
  author,
  step_duration = 180,
  steady_state_duration = 60,
  RER_cutoff = NULL
)
```

## Arguments

- step_time:

  how often the data was collected (in seconds).

- db_MFO:

  data frame with MFO test (columns: VO2, VCO2, RER, HR).

- VO2max:

  maximum oxygen uptake in ml/min.

- author:

  equation set: "Frayn", "Jeukendrup", or "Peronnet".

- step_duration:

  duration of each exercise step in seconds (default 180 = 3 min).

- steady_state_duration:

  duration of steady-state period to average within each step, in
  seconds (default 60 = last minute).

- RER_cutoff:

  RER value above which data is excluded (default NULL = no cutoff).

## Value

data frame with columns: VO2, VCO2, HR, RER, CHO, FAT, Kcal (one row per
step)

## Details

With \`author = "Jeukendrup"\`, a single warning lists the steps (after
\`RER_cutoff\`) whose intensity is outside 40-75 % VO2max, the range of
the Jeukendrup & Wallis (2005, Table 5) equations (SPEC 3, 9.5).
