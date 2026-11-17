# Package index

## Import data

Read a metabolic cart export into the tables that
[`MFO()`](https://jorgedelro.github.io/MFO/reference/MFO.md) expects
(internal data contract, 10-s averages).

- [`read_cart()`](https://jorgedelro.github.io/MFO/reference/read_cart.md)
  : Read a metabolic cart export

## Describe the test

Protocol of the MFO test (step durations, averaged segment, load and
exercise mode) and VO2max of the graded test with maximality criteria.

- [`MFO_protocol()`](https://jorgedelro.github.io/MFO/reference/MFO_protocol.md)
  : Describe the protocol of an MFO test
- [`VO2max_from_test()`](https://jorgedelro.github.io/MFO/reference/VO2max_from_test.md)
  : VO2max of a graded test, with maximality criteria

## Compute MFO and Fatmax

One participant: basal window, step averaging, stoichiometric equation,
curve fit (P3, P2, MV, MRER), Fatmax zone and crossover point.

- [`MFO()`](https://jorgedelro.github.io/MFO/reference/MFO.md) : Maximal
  Fat Oxidation & FatMax Function

## Kinetics

SIN model of fat oxidation kinetics (Chenevière et al., 2009).

- [`MFO_kinetics()`](https://jorgedelro.github.io/MFO/reference/MFO_kinetics.md)
  : Maximal Fat Oxidation Kinetics (SIN Model)

## Batch

Many participants from a folder of xlsx files.

- [`MFOs()`](https://jorgedelro.github.io/MFO/reference/MFOs.md) :
  Maximal Fat Oxidation calculation of multiple databases

## Reliability

Test-retest reliability of a lab: ICC(2,1), SEM, MDC, CV, bias and
limits of agreement (Bland-Altman plots).

- [`MFO_reliability()`](https://jorgedelro.github.io/MFO/reference/MFO_reliability.md)
  : Test-retest reliability of MFO outcomes
- [`plot(`*`<MFO_reliability>`*`)`](https://jorgedelro.github.io/MFO/reference/plot.MFO_reliability.md)
  : Bland-Altman plots of a reliability analysis

## Simulate

Synthetic repeated tests with a known truth, for reliability and
uncertainty studies and demonstrations.

- [`simulate_MFO_sessions()`](https://jorgedelro.github.io/MFO/reference/simulate_MFO_sessions.md)
  : Simulate repeated MFO tests with a known truth
- [`write_MFO_sessions()`](https://jorgedelro.github.io/MFO/reference/write_MFO_sessions.md)
  : Write simulated sessions as folders for MFOs()

## Datasets

Anonymised example recordings of one participant (20-s averages).

- [`basal_df`](https://jorgedelro.github.io/MFO/reference/basal_df.md) :
  Basal test dataframe
- [`MFO_df`](https://jorgedelro.github.io/MFO/reference/MFO_df.md) : MFO
  test dataframe
- [`VO2max_df`](https://jorgedelro.github.io/MFO/reference/VO2max_df.md)
  : Graded exercise test dataframe

## Internal helpers

Not exported; documented so that every step of
[`MFO()`](https://jorgedelro.github.io/MFO/reference/MFO.md) can be
traced. Their arguments may change without notice.

- [`read_MFO_databases()`](https://jorgedelro.github.io/MFO/reference/read_MFO_databases.md)
  : Read databases for MFO package
- [`preprocess_bxb()`](https://jorgedelro.github.io/MFO/reference/preprocess_bxb.md)
  : Preprocess breath-by-breath data into fixed intervals
- [`met_basal()`](https://jorgedelro.github.io/MFO/reference/met_basal.md)
  : Basal metabolic rate
- [`calculate_vars()`](https://jorgedelro.github.io/MFO/reference/calculate_vars.md)
  : Calculate step means from raw data
- [`substrate_oxidation()`](https://jorgedelro.github.io/MFO/reference/substrate_oxidation.md)
  : Substrate oxidation equations
- [`fat_oxidation_MRER()`](https://jorgedelro.github.io/MFO/reference/fat_oxidation_MRER.md)
  : Fat oxidation with the RER simplification (MRER)
