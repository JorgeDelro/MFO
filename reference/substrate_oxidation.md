# Substrate oxidation equations

Calculate fat and carbohydrate oxidation from VO2 and VCO2 using
different stoichiometric equations.

## Usage

``` r
substrate_oxidation(
  VO2,
  VCO2,
  author,
  VO2max = NULL,
  phase = c("exercise", "rest")
)
```

## Arguments

- VO2:

  oxygen consumption in ml/min (scalar or vector)

- VCO2:

  carbon dioxide production in ml/min (same length as VO2)

- author:

  equation set: "Frayn", "Jeukendrup", or "Peronnet"

- VO2max:

  maximum oxygen uptake in ml/min (required for Jeukendrup during
  exercise)

- phase:

  "exercise" (default) or "rest". Only changes "Jeukendrup".

## Value

Named list with CHO (g/min), FAT (g/min), Kcal (kcal/min)

## Details

Rule: a coefficient printed in the source is used as printed; a derived
coefficient is used only where the source prints no equation (SPEC 19).

\*\*Jeukendrup & Wallis (2005)\*\* (Int J Sports Med 26 Suppl 1:
S28-S37), with the coefficients printed in their Table 5:

- below 50 % VO2max (40-50 % set): CHO = 4.344 VCO2 - 3.061 VO2;

- at or above 50 % VO2max (50-75 % set): CHO = 4.210 VCO2 - 2.962 VO2;

- FAT = 1.695 VO2 - 1.701 VCO2 in both.

With \`phase = "rest"\` there is no printed equation: Jeukendrup &
Wallis propose none. As a package decision (pure glucose, no glycogen
use at rest), the pair is derived from the O2/CO2 constants of their
Table 4 (glucose 0.7455/0.7426, fatty acid 2.0092/1.4136 L/g) by solving
the 2x2 system: CHO = 4.58519 VCO2 - 3.22597 VO2, FAT = 1.69469 VO2 -
1.70130 VCO2. The same derivation with 50/50 and 20/80 glucose/glycogen
reproduces the Table 5 coefficients within 0.005 and is kept as a
cross-check in the tests.

The Table 5 sets are defined for 40-75 % VO2max only. Below 40 % the
40-50 % set and above 75 % the 50-75 % set are extrapolated;
\[calculate_vars()\] warns when an exercise step falls outside the range
(SPEC 3, 9.5). Resting values (\`phase = "rest"\`) never trigger that
warning.
