# Maximal Fat Oxidation Kinetics (SIN Model)

Fit the SIN model (Cheneviere et al., 2009) to describe fat oxidation
kinetics and extract three parameters: dilatation (d), symmetry (s), and
translation (t). Also calculates Fatmin (the intensity where fat
oxidation reaches zero), which is a unique capability of the SIN model
over polynomial methods.

## Usage

``` r
MFO_kinetics(MFO_data, fatmin_threshold = NULL)
```

## Arguments

- MFO_data:

  a data frame obtained from the MFO function (must contain FAT and
  porc_VO2 columns).

- fatmin_threshold:

  \`NULL\` (default) for Fatmin at the second zero of the curve, or a
  proportion of MFO in (0, 1), e.g. 0.05, for the intensity above Fatmax
  where fat oxidation falls to that fraction of MFO (closed form:
  \\(pi + 2d) ((pi - asin(threshold)) / pi)^(1/s) - d - t\\ / K). v0.2.0
  used a fixed 5 % threshold searched on a grid.

## Value

Returns a list which contains:

- MFO_kinetics_data: \`MFO_data\` plus normalized %MFO (\`porc_MFO\`,
  100 = highest observed FAT).

- MFO_kinetics_plot: ggplot object with the observed %MFO, the basic SIN
  and the fitted SIN.

- d: dilatation parameter (width of the curve).

- t: translation parameter (horizontal shift along intensity axis).

- s: symmetry parameter (controls curve shape asymmetry; \<1 left-skew,
  \>1 right-skew).

- Fatmin: exercise intensity (% VO2max) at the second zero of the curve,
  or at \`fatmin_threshold\` of MFO if given.

- FatMax_SIN: exercise intensity (% VO2max) at peak fat oxidation from
  SIN model.

- R_squared: R-squared of the SIN model fit (observed proportions of
  MFO).

- RMSE: Root mean squared error of the SIN model fit (proportion of
  MFO).

- d_se: standard error of dilatation parameter.

- s_se: standard error of symmetry parameter.

- t_se: standard error of translation parameter.

- convergence: logical, whether the SIN model converged.

## Details

The model is equation \\14\\ of Cheneviere et al. (2009, Med Sci Sports
Exerc 41: 1615-1625, p. 1618,
[doi:10.1249/MSS.0b013e31819e2f91](https://doi.org/10.1249/MSS.0b013e31819e2f91)
): \$\$\\MFO = \sin\\\[\pi^{1/s} / (\pi + 2d) \cdot (Kx + d + t)\]^s\\,
\quad K = \pi/100,\$\$ x in % VO2max. Up to v0.2.0 the package computed
(pi^(1/s)/pi) + 2d, so its \`d\` (and \`t\`) are not comparable with
published values (SPEC 6.5, 19).

## Fitting procedure

As in Cheneviere et al. (2009), the model is fitted by non-linear least
squares (Levenberg-Marquardt, \[minpack.lm::nlsLM()\], start d = 0, s =
1, t = 0) directly to the observed rows of \`MFO_data\`, with fat
oxidation expressed as a proportion of the highest observed value (%MFO
/ 100). There is no smoothing and no synthetic grid: v0.1.0/v0.2.0
fitted the SIN to a fine grid predicted by a third-degree polynomial,
which inflated n and shrank the standard errors of d, s and t (SPEC 19,
finding 7). R-squared and RMSE are computed on the observed proportions.
At least 4 rows are needed for 3 parameters: with fewer the function
warns and returns \`convergence = FALSE\`; with 4 or 5 it fits but warns
that the fit is unreliable and that at least 6 steps are recommended
(SPEC 6.5).

\`FatMax_SIN\` and \`Fatmin\` come from closed forms derived from eq.
\\14\\: Fatmax = \\(pi + 2d) 2^(-1/s) - d - t\\ / K and, by default,
Fatmin = second zero of the curve = (pi + d - t) / K, Cheneviere's
definition (SPEC 19, finding 3). Fatmin can lie beyond the last observed
step (extrapolation).

## Which rows are fitted

\`MFO_kinetics()\` fits every row it receives. Cheneviere et al. (2009)
fitted exercise steps only, and averaged only steps with RER \<= 1.0:

- \`MFO(..., RER_cutoff = 1)\` reproduces their step selection;

- \`MFO(..., include_basal = TRUE)\` adds the resting row to \`MFO_db\`,
  so it is also fitted here. This departs from Cheneviere's procedure in
  the SIN as well as in the P3.

The SIN is sensitive to the last steps because the curve must reach
zero: in the example data (\`author = "Frayn"\`) the last step (74.7 %
VO2max, RER 1.03) has negative fat oxidation.

- \`negative_FAT = "truncate"\` (default) sets it to 0 and that point
  anchors the second zero: d = -0.385, s = 0.910, t = 0.355, FatMax_SIN
  = 36.2, Fatmin = 76.4 % VO2max.

- \`negative_FAT = "exclude"\` or \`RER_cutoff = 1\` drops it: d =
  -0.440, s = 0.539, t = 0.023, FatMax_SIN = 33.2, Fatmin = 85.3 %
  VO2max.

Report which option was used when publishing SIN parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read dfs
data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")

# Convert to data.frame
basal_df <- data.frame(basal_df)
MFO_df <- data.frame(MFO_df)
VO2max_df <- data.frame(VO2max_df)

# Calculate MFO and Fatmax
result_MFO <- MFO(step_time = 20,
                 db_MFO = MFO_df,
                 db_basal = basal_df,
                 db_graded = VO2max_df,
                 cv_var = "RER",
                 author = "Frayn")

# Calculate MFO Kinetics
result_kinetics <- MFO_kinetics(result_MFO$MFO_db)

# Fatmin at 5 % of MFO instead of the second zero
MFO_kinetics(result_MFO$MFO_db, fatmin_threshold = 0.05)$Fatmin
} # }
```
