# Methods paragraph of the report (#24): the template approved by Jorge on
# 2026-11-20, sentence by sentence, with the sources of SPEC 19

.fake_analysis <- function(..., averaging = "none (records as exported)", converged = TRUE,
                           VO2max_type = "VO2max") {
  o <- utils::modifyList(.app_default_opts(), list(...))
  list(opts = o,
       res = list(VO2max = 3012.4, VO2max_window = 40, VO2max_type = VO2max_type),
       kin = list(convergence = converged),
       cart = list(step_time = 20, info = list(averaging = averaging)))
}
.has <- function(m, x) grepl(x, m$text, fixed = TRUE)
.ref <- function(m, x) any(grepl(x, m$references, fixed = TRUE))

test_that("the default analysis gives the approved paragraph", {
  m <- .app_methods(.fake_analysis())
  expect_true(startsWith(m$text, "[Participants were tested after {...} h of fasting, on a cycle ergometer.]"))
  expect_true(.has(m, sprintf("R package MFO (version %s)", utils::packageVersion("MFO"))))
  expect_true(.has(m, "used as exported by the analyser (20-s averages)"))
  expect_true(.has(m, "lowest coefficient of variation of RER"))
  expect_true(.has(m, "over the last 60 s of each 180-s step."))
  expect_true(.has(m, "stoichiometric equations of Frayn (1983), assuming negligible protein oxidation."))
  expect_true(.has(m, "Negative fat oxidation values were set to zero."))
  expect_true(.has(m, "highest 40-s rolling mean of VO\u2082 in the graded test and was labelled VO\u2082max according to Howley et al. (1995)."))
  expect_true(.has(m, "determined from a third-degree polynomial fitted to fat oxidation against % VO\u2082max."))
  expect_true(.has(m, "at least 90 % of MFO (Achten et al., 2002)"))
  expect_true(.has(m, "(Brooks and Mercier, 1994)"))
  expect_true(.has(m, "SIN model (Chenevière et al., 2009), fitted to fat oxidation normalised to MFO"))
  expect_false(.has(m, "Energy expenditure"))
  expect_false(.has(m, "confidence intervals"))
  expect_false(grepl("seed|commit", m$text))
  expect_true(grepl("^Fernandez-Santos JR", m$references[1]))
  expect_true(.ref(m, "Frayn KN (1983)") && .ref(m, "Howley ET") && .ref(m, "Achten J") &&
              .ref(m, "Brooks GA") && .ref(m, "Chenevière X"))
  expect_false(.ref(m, "Péronnet") || .ref(m, "Gelman") || .ref(m, "Davison"))
})

test_that("each option changes its own sentence", {
  m <- .app_methods(.fake_analysis(mode = "walking", method = "MV", converged = FALSE,
                                   negative_FAT = "exclude", averaging = "mean of breaths per interval"))
  expect_true(.has(m, "on a treadmill.]"))
  expect_true(.has(m, "MFO and Fatmax were determined as the step with the highest measured fat oxidation."))
  expect_true(.has(m, "Steps with negative fat oxidation were excluded."))
  expect_true(.has(m, "averaged in consecutive 20-s intervals"))
  expect_false(.has(m, "SIN model"))
  m <- .app_methods(.fake_analysis(method = "MRER"))
  expect_true(.has(m, "determined with the MRER method (Chenevière et al., 2009)."))
  m <- .app_methods(.fake_analysis(method = "P2", include_basal = TRUE))
  expect_true(.has(m, "second-degree polynomial fitted to fat oxidation against % VO\u2082max, including the resting value as the first point."))
  m <- .app_methods(.fake_analysis(RER_cutoff = "safe"))
  expect_true(.has(m, "Steps with RER above 0.95 were excluded (Amaro-Gahete et al., 2019)."))
  expect_true(.ref(m, "Amaro-Gahete FJ, Sanchez-Delgado G, Alcantara"))
  m <- .app_methods(.fake_analysis(RER_cutoff = "classic"))
  expect_true(.has(m, "above 1.00 were excluded (Chenevière et al., 2009)."))
  m <- .app_methods(.fake_analysis(RER_cutoff = "custom", RER_custom = 0.97))
  expect_true(.has(m, "Steps with RER above 0.97 were excluded."))
  m <- .app_methods(.fake_analysis(author = "Frayn", energy_equivalents = "source"))
  expect_true(.has(m, "energy equivalents of Péronnet and Massicotte (1991)"))
  expect_true(.has(m, "as Frayn (1983) gives none."))
  expect_true(.ref(m, "Péronnet F"))
  m <- .app_methods(.fake_analysis(author = "Jeukendrup", energy_equivalents = "source"))
  expect_true(.has(m, "Jeukendrup and Wallis (2005)"))
  expect_true(.has(m, "3.95 kcal/g below and 4.07 kcal/g from 50 % VO\u2082max"))
  m <- .app_methods(.fake_analysis(author = "Peronnet", energy_equivalents = "source"))
  expect_true(.has(m, "of the same source (3.8683 kcal/g"))
  m <- .app_methods(.fake_analysis(vo2max_source = "value"))
  expect_true(.has(m, "VO\u2082max (3012 mL/min) was measured in a separate test."))
  expect_false(.ref(m, "Howley"))
  m <- .app_methods(.fake_analysis(type = "step_variable", step_duration = "300, 180"))
  expect_true(.has(m, "of each step (step durations: 300, 180 s, the last one repeated)."))
  m <- .app_methods(.fake_analysis(type = "ramp", ramp_window = 30, ramp_step = 15))
  expect_true(.has(m, "averaged in 30-s windows every 15 s of the ramp."))
  m <- .app_methods(.fake_analysis(sin_fit = "absolute"))
  expect_true(.has(m, "fitted to absolute fat oxidation"))
})

test_that("intervals and zones sentences, with their sources", {
  a <- .fake_analysis()
  m <- .app_methods(a, ci = list(type = "parametric", B = 2000, conf = 0.95, seed = 7))
  expect_true(.has(m, "95 % confidence intervals were obtained by a parametric bootstrap with σ drawn from its posterior under a flat prior (Gelman et al., 2013) with 2000 replicates."))
  expect_false(.has(m, "7"))
  m <- .app_methods(a, ci = list(type = "bootstrap", B = 500, conf = 0.9))
  expect_true(.has(m, "90 % confidence intervals were obtained by a bootstrap of the modified residuals (Davison and Hinkley, 1997) with 500 replicates."))
  zn <- function(...) utils::modifyList(list(HRmax = 185, HRmax_source = "measured",
                                             obesity_range_lower = NA, MFO_percentile_label = NA,
                                             Fatmax_percentile_label = NA,
                                             reference = "Amaro-Gahete 2018: Sedentary adults",
                                             reference_n = 167L,
                                             reference_differences = "method: P3; fasting: 5-6 h"),
                                        list(...))
  m <- .app_methods(a, zones = list(zones = zn(), options = list(population = "general")))
  expect_true(.has(m, "expressed as a percentage of the measured peak heart rate."))
  expect_false(.has(m, "percentiles"))
  m <- .app_methods(a, zones = list(zones = zn(HRmax_source = "predicted (Tanaka)"),
                                    options = list(population = "general")))
  expect_true(.has(m, "the predicted maximal heart rate (Tanaka et al., 2001)."))
  expect_true(.ref(m, "Tanaka H"))
  m <- .app_methods(a, zones = list(zones = zn(HRmax_source = "given", obesity_range_lower = 61,
                                               MFO_percentile_label = "P44"),
                                    options = list(population = "obesity")))
  expect_true(.has(m, "the maximal heart rate provided."))
  expect_true(.has(m, "compared with the range recommended for obesity by Chávez-Guevara et al. (2023)."))
  expect_true(grepl("percentiles of sedentary adults of Amaro-Gahete et al. \\(2018\\) \\(Sedentary adults, n = 167\\); that reference differs from this test in method: P3; fasting: 5-6 h\\.$", m$text))
  expect_true(.ref(m, "Chávez-Guevara IA") && .ref(m, "Ruiz JR (2018)"))
  m <- .app_methods(a, zones = list(zones = zn(HRmax = NA), options = list(population = "general")))
  expect_false(.has(m, "Heart rate at Fatmax"))
})

test_that("the package citation and the commit", {
  expect_match(.app_package_citation(), "MFO")
  commit <- .app_commit()
  expect_true(is.na(commit) || nchar(commit) == 7)
})
