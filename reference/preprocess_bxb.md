# Preprocess breath-by-breath data into fixed intervals

Averages breath-by-breath data into fixed time intervals.

## Usage

``` r
preprocess_bxb(db, target_interval = 20)
```

## Arguments

- db:

  data frame with breath-by-breath data. Must contain a "Time" column in
  seconds and columns for VO2, VCO2, RER, HR.

- target_interval:

  target interval in seconds (default 20).

## Value

data frame with data averaged into fixed intervals.
