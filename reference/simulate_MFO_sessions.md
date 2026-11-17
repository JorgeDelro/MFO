# Simulate repeated MFO tests with a known truth

Generates synthetic MFO tests (resting measure, MFO test and graded
test) for several participants and sessions, with known true MFO, Fatmax
and SIN parameters. It is meant for testing the reliability (SPEC 12)
and uncertainty (SPEC 13) modules and for demonstrations, not as a model
of physiology.

## Usage

``` r
simulate_MFO_sessions(
  n_participants = 10,
  n_sessions = 2,
  population,
  shape = "basic",
  cv_MFO = 0.21,
  cv_Fatmax = 0.26,
  familiarization = 0,
  noise_cv = 0,
  noise_cor = 0,
  protocol = MFO_protocol(),
  n_steps = 8,
  intensity = c(20, 75),
  step_time = 20,
  author = c("Frayn", "Jeukendrup", "Peronnet"),
  hr_rest = 60,
  age = 30,
  seed = NULL
)
```

## Arguments

- n_participants:

  number of participants.

- n_sessions:

  number of sessions per participant.

- population:

  list with \`VO2max\` (mean and SD, mL/min; required) and optionally
  \`MFO\` (g/min) and \`Fatmax\` (% VO2max), each \`c(mean, sd)\`. An SD
  of 0 gives the same value to every participant.

- shape:

  \`"basic"\` (d = 0, s = 1; default), \`"trained"\` (d = 0.18, s =
  1.52, the trained participant of Cheneviere et al. 2009, Fig. 5), or a
  list with \`d\` and \`s\`, each one number or one per participant.

- cv_MFO, cv_Fatmax:

  between-session coefficients of variation (proportions; defaults 0.21
  and 0.26).

- familiarization:

  relative bias of session 1, one number for MFO and Fatmax or \`c(MFO =
  , Fatmax = )\` (default 0).

- noise_cv, noise_cor:

  analyser noise per record: coefficient of variation of VO2 and VCO2
  and their correlation (defaults 0).

- protocol:

  an \[MFO_protocol()\] of type \`"step"\`, \`"step_variable"\` or
  \`"relative"\`.

- n_steps:

  number of exercise steps (default 8).

- intensity:

  % VO2max of the first and last step (default 20 and 75).

- step_time:

  record interval in seconds (default 20); step durations and the
  averaged segment must be multiples of it.

- author:

  equation whose inverse gives VCO2: "Frayn" (default), "Jeukendrup" or
  "Peronnet". Analyse the data with the same \`author\` to recover the
  truth.

- hr_rest, age:

  resting heart rate (bpm) and age (years) for the heart-rate line
  (defaults 60 and 30).

- seed:

  optional integer for reproducible results; the random state of the
  session is restored afterwards.

## Value

An object of class \`MFO_sim\`: a list with

- \`data\`: \`data\$P01\$S1\` is a list with \`db_basal\`, \`db_MFO\`
  and \`db_graded\`, ready for \[MFO()\] with \`step_time\`;

- \`truth\`: one row per participant and session with \`id\`,
  \`session\`, \`VO2max\`, \`MFO\`, \`Fatmax\`, \`d\`, \`s\`, \`t\` and
  \`Fatmin\` (second zero of the SIN curve);

- \`participants\`: the participants' values before the between-session
  variation;

- \`settings\`: the arguments, including \`step_time\` and \`protocol\`.

## Details

\*\*How a test is built.\*\* Each participant has a true VO2max, MFO and
curve shape. Fat oxidation follows the SIN model of Cheneviere et al.
(2009,
[doi:10.1249/MSS.0b013e31819e2f91](https://doi.org/10.1249/MSS.0b013e31819e2f91)
), FAT = MFO x SIN(x; d, s, t), truncated at 0, with x the step
intensity (% VO2max); t is set so that the maximum falls at the
participant's Fatmax. Each step has VO2 = x / 100 x VO2max and the VCO2
for which the \`author\` equation gives that FAT, so \[MFO()\] recovers
the curve exactly when there is no noise. VO2 is constant within a step
(no on-kinetics). Steps are cut as in \`protocol\`, which also gives the
load schedule (\`Load\` column only when \`load_start\` and
\`load_increment\` are set).

\*\*Variability.\*\* Three levels:

- between participants: MFO (log-normal), Fatmax (normal, truncated to
  \`intensity\`) and VO2max (normal, positive) from \`population\`. The
  MFO and Fatmax defaults are the sedentary adults of Amaro-Gahete,
  Sanchez-Delgado and Ruiz (2018): 0.34 +/- 0.10 g/min and 44.2 +/- 12.4
  % VO2max. VO2max has no default and must be given.

- between sessions: each session multiplies the participant's MFO and
  Fatmax by log-normal factors with coefficients of variation \`cv_MFO\`
  and \`cv_Fatmax\`. The defaults, 21 % and 26 %, are the total
  test-retest CVs reported by Chrzanowski-Smith et al. (2020, n = 99),
  which include biological, analyser and fitting error; used here as
  day-to-day variation, and with analyser noise on top, they make the
  simulated test-retest variability an upper bound. For Fatmax, Croci et
  al. (2014, n = 15) report CVs of 16.4 % (SIN), 18.6 % (P3) and 20.8 %
  (MV). \`familiarization\` shifts session 1 only (a relative bias).

- within a session: every record of VO2 and VCO2 is multiplied by (1 +
  e), with e normal, standard deviation \`noise_cv\` and correlation
  \`noise_cor\` between the two gases. There is no recommended value:
  the default 0 gives noiseless tests, and studies of the fit
  uncertainty set it explicitly as a design parameter.

\*\*Resting and graded tests.\*\* Conveniences for \[MFO()\], not
physiological claims: the resting measure lasts 15 min at 10 % VO2max
with RER 0.80; the graded test has 60-s stages from 50 to 100 % VO2max
with RER rising linearly from 0.90 to 1.15, so the last stage gives
VO2max exactly. Heart rate rises linearly with % VO2max from \`hr_rest\`
to the predicted HRmax of Tanaka, Monahan and Seals (2001,
[doi:10.1016/s0735-1097(00)01054-8](https://doi.org/10.1016/s0735-1097%2800%2901054-8)
), 208 - 0.7 x \`age\`.

## References

Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:
Contextualising maximal fat oxidation during exercise: determinants and
normative values. Front Physiol 9: 1460, Table 1 (sedentary adults).
[doi:10.3389/fphys.2018.01460](https://doi.org/10.3389/fphys.2018.01460)

Chrzanowski-Smith OJ, Edinburgh RM, Thomas MP, et al. (2020). The
day-to-day reliability of peak fat oxidation and FATMAX. Eur J Appl
Physiol 120: 1745-1759.
[doi:10.1007/s00421-020-04397-3](https://doi.org/10.1007/s00421-020-04397-3)

Croci I, Borrani F, Byrne NM, et al. (2014). Reproducibility of Fatmax
and fat oxidation rates during exercise in recreationally trained males.
PLoS ONE 9: e97930.
[doi:10.1371/journal.pone.0097930](https://doi.org/10.1371/journal.pone.0097930)

## See also

\[write_MFO_sessions()\], \[MFO()\], \[MFO_kinetics()\]

## Examples

``` r
sim <- simulate_MFO_sessions(n_participants = 3, n_sessions = 2,
                             population = list(VO2max = c(3000, 500)),
                             seed = 1)
sim
#> <MFO_sim> 3 participant(s) x 2 session(s), 8 steps of step, 20-s records (Frayn)
#>   between sessions: CV MFO 0.21, CV Fatmax 0.26; familiarization MFO 0, Fatmax 0
#>   analyser noise: CV 0, correlation 0
#>   true MFO 0.222-0.548 g/min, Fatmax 41.5-71.6 % VO2max
sim$truth
#>    id session   VO2max       MFO   Fatmax d s           t    Fatmin
#> 1 P01      S1 2686.773 0.4743505 71.58174 0 1 -0.67801032 121.58174
#> 2 P01      S2 2686.773 0.5480535 41.48361 0 1  0.26755014  91.48361
#> 3 P02      S1 3091.822 0.2215653 68.85326 0 1 -0.59229272 118.85326
#> 4 P02      S2 3091.822 0.3477422 51.42501 0 1 -0.04476787 101.42501
#> 5 P03      S1 2582.186 0.3066262 61.30081 0 1 -0.35502533 111.30081
#> 6 P03      S2 2582.186 0.2851269 62.85278 0 1 -0.40378191 112.85278
s1 <- sim$data$P01$S1
r <- MFO(step_time = sim$settings$step_time, db_MFO = s1$db_MFO,
         db_basal = s1$db_basal, db_graded = s1$db_graded, cv_var = "RER",
         author = "Frayn", energy_equivalents = "classic", age = 30)
#> VO2max: a 30-s window is not a multiple of step_time (20 s); using 2 record(s) = 40 s.
c(estimated = r$FAT_MAX, true = sim$truth$Fatmax[1])
#> estimated      true 
#>  70.41290  71.58174 
```
