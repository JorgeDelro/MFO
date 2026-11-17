# Bland-Altman plots of a reliability analysis

Difference against mean of one pair of consecutive sessions, with the
bias and the limits of agreement, one panel per variable (Bland & Altman
1986).

## Usage

``` r
# S3 method for class 'MFO_reliability'
plot(x, pair = 1, variables = NULL, ...)
```

## Arguments

- x:

  result of \[MFO_reliability()\].

- pair:

  which pair of consecutive sessions: 1 (default) is S2 - S1, 2 is S3 -
  S2, and so on.

- variables:

  variables to draw (default all).

- ...:

  not used.

## Value

A ggplot object.
