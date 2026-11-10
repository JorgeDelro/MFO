# Substrate oxidation equations

Calculate fat and carbohydrate oxidation from VO2 and VCO2 using
different stoichiometric equations.

## Usage

``` r
substrate_oxidation(VO2, VCO2, author, VO2max = NULL)
```

## Arguments

- VO2:

  oxygen consumption in ml/min

- VCO2:

  carbon dioxide production in ml/min

- author:

  equation set: "Frayn", "Jeukendrup", or "Peronnet"

- VO2max:

  maximum oxygen uptake in ml/min (required for Jeukendrup)

## Value

Named list with CHO (g/min), FAT (g/min), Kcal (kcal/min)
