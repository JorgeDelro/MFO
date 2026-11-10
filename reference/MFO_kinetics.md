# Maximal Fat Oxidation Kinetics (SIN Model)

Fit the SIN model (Cheneviere et al., 2009) to describe fat oxidation
kinetics and extract three parameters: dilatation (d), symmetry (s), and
translation (t). Also calculates Fatmin (the intensity where fat
oxidation becomes negligible), which is a unique capability of the SIN
model over polynomial methods.

## Usage

``` r
MFO_kinetics(MFO_data)
```

## Arguments

- MFO_data:

  a data frame obtained from the MFO function (must contain FAT and
  porc_VO2 columns).

## Value

Returns a list which contains:

- MFO_kinetics_data: database used to create the MFO kinetics plot
  (original data plus normalized %MFO).

- MFO_kinetics_plot: ggplot object comparing basic and fitted SIN
  models.

- d: dilatation parameter (controls vertical scaling/width of the
  curve).

- t: translation parameter (horizontal shift along intensity axis).

- s: symmetry parameter (controls curve shape asymmetry; \<1 left-skew,
  \>1 right-skew).

- Fatmin: exercise intensity (% VO2max) at which fat oxidation becomes
  negligible.

- FatMax_SIN: exercise intensity (% VO2max) at peak fat oxidation from
  SIN model.

- R_squared: R-squared of the SIN model fit.

- RMSE: Root mean squared error of the SIN model fit.

- d_se: standard error of dilatation parameter.

- s_se: standard error of symmetry parameter.

- t_se: standard error of translation parameter.

- convergence: logical, whether the SIN model converged.

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
} # }
```
