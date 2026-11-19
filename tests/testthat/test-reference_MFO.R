# Expected values: Table 1 of Amaro-Gahete, Sanchez-Delgado and Ruiz (2018),
# Front Physiol 9:1460, as printed (exact; tolerance 1e-12)

test_that("reference_MFO reproduces Table 1 of Amaro-Gahete 2018", {
  ref <- MFO::reference_MFO
  expect_equal(nrow(ref), 12)
  all <- ref[ref$grouping == "All", ]
  expect_equal(all$n, 167L)
  expect_equal(unlist(all[c("MFO_mean", "MFO_sd")], use.names = FALSE),
               c(0.34, 0.10), tolerance = 1e-12)
  expect_equal(unlist(all[paste0("MFO_p", c(20, 40, 60, 80))], use.names = FALSE),
               c(0.24, 0.30, 0.35, 0.42), tolerance = 1e-12)
  expect_equal(unlist(all[paste0("Fatmax_p", c(20, 40, 60, 80))], use.names = FALSE),
               c(33.2, 39.6, 44.6, 54.1), tolerance = 1e-12)
  mw <- ref[ref$sex %in% "female" & ref$age_group %in% "middle-aged", ]
  expect_equal(mw$n, 23L)
  expect_equal(unlist(mw[paste0("Fatmax_p", c(20, 40, 60, 80))], use.names = FALSE),
               c(39.0, 42.7, 50.1, 54.4), tolerance = 1e-12)
  ob <- ref[ref$weight_status %in% "obese", ]
  expect_equal(c(ob$n, ob$MFO_p60, ob$Fatmax_mean), c(29, 0.39, 43.3), tolerance = 1e-12)
})

test_that("reference_MFO groups add up to the whole cohort", {
  ref <- MFO::reference_MFO
  for (g in c("By Sex", "By Age", "By Sex and Age", "By BMI Status")) {
    expect_equal(sum(ref$n[ref$grouping == g]), 167L, label = g)
  }
  for (v in c("MFO", "Fatmax")) {
    p <- as.matrix(ref[paste0(v, "_p", c(20, 40, 60, 80))])
    expect_true(all(apply(p, 1, function(x) all(diff(x) >= 0))), label = v)
  }
})
