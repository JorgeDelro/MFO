# MFO test dataframe

Synthetic breath-by-breath data from an incremental exercise test with 8
stages (25-200 W, 3 min per stage). Simulates a moderately trained male
cyclist covering ~20-75% VO2max. The fat oxidation pattern shows a clear
inverted U-shape with a peak around 35-40% VO2max, suitable for
demonstrating P3, P2, MRER, and SIN model fitting.

## Usage

``` r
data(MFO_df)
```

## Format

A data frame with 72 rows and 8 variables:

- Time:

  time test, in seconds (20s intervals)

- HR:

  heart rate, in beats/min

- VO2:

  volume of oxygen consumption, in mL/min

- VCO2:

  volume of exhaled carbon dioxide, in mL/min

- RER:

  respiratory exchange ratio

- BF:

  breathing frequency, in breaths/min

- VE:

  ventilation, in L/min

- PETCO2:

  end-tidal carbon dioxide pressure, in mmHg
