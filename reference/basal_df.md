# Basal test dataframe

Synthetic breath-by-breath data from a 15-minute resting metabolic
assessment. Simulates a moderately trained male cyclist (VO2max ~3200
mL/min) with an initial stabilization period followed by steady-state
resting metabolism.

## Usage

``` r
data(basal_df)
```

## Format

A data frame with 45 rows and 8 variables:

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
