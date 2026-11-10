# Graded exercise test dataframe

Synthetic breath-by-breath data from a graded exercise test to
exhaustion (10 stages, 75-300 W). Simulates a moderately trained male
cyclist reaching VO2max ~3200 mL/min and HRmax ~185 bpm. Used to
determine VO2max when this value is not known a priori.

## Usage

``` r
data(VO2max_df)
```

## Format

A data frame with 30 rows and 9 variables:

- Time:

  time test, in seconds (20s intervals)

- HR:

  heart rate, in beats/min

- Load:

  workload, in watts

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
