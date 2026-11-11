# Fat oxidation with the RER simplification (MRER)

FAT (mg/min) = 1.7 (1 - RER) VO2 (ml/min), the simplification of the
Peronnet & Massicotte (1991) equation used by Perez-Martin et al. (2001)
and Cheneviere et al. (2009, p. 1617). Used by \`method = "MRER"\` in
\[MFO()\].

## Usage

``` r
fat_oxidation_MRER(VO2, RER)
```

## Arguments

- VO2:

  vector of oxygen consumption values (ml/min)

- RER:

  vector of respiratory exchange ratio values

## Value

fat oxidation rate in g/min
