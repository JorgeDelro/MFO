# Prior predictive check of MFO_bayes()

Samples the priors of \[MFO_bayes()\] only (\`sample_prior = "only"\` in
brms) and shows the fat oxidation curves they imply, with the observed
steps for reference, and the prior distribution of MFO, Fatmax and
Fatmin. Use it to check that the priors allow every plausible curve and
few absurd ones before fitting, and, against an \[MFO_bayes()\] result,
to see which parameters the data inform (compare the prior and posterior
of d).

## Usage

``` r
MFO_prior_check(
  MFO_data,
  model = c("SIN", "P3"),
  priors = NULL,
  sigma = c("constant", "proportional"),
  fatmin_prior = c(90, 14),
  ndraws = 200,
  seed = NULL,
  backend = "rstan",
  ...
)
```

## Arguments

- MFO_data:

  the \`MFO_db\` of an \[MFO()\] result (columns \`porc_VO2\` and
  \`FAT\`); every row is used, as in \[MFO_kinetics()\].

- model:

  \`"SIN"\` (default) or \`"P3"\`.

- priors:

  optional \`brmsprior\` object replacing all default priors.

- sigma:

  \`"constant"\` (default) or \`"proportional"\` to intensity.

- fatmin_prior:

  mean and SD of the normal prior of Fatmin, truncated to (Fatmax, 100)
  (default \`c(90, 14)\`, SIN only).

- ndraws:

  number of prior curves drawn (default 200).

- seed:

  optional integer passed to Stan.

- backend:

  passed to \[brms::brm()\] (default \`"rstan"\`).

- ...:

  further arguments to \[brms::brm()\].

## Value

An object of class \`MFO_prior_check\`: a list with \`draws\` (prior
draws of the parameters and derived quantities), \`summary\` (quantiles
5, 50 and 95 %), \`curves\` (ndraws curves on 0-100 % VO2max), \`data\`
and \`model\`. \`plot()\` draws the curves.

## See also

\[MFO_bayes()\]
