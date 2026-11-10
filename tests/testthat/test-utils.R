# Hand calculations: constant resting data VO2 = 300, VCO2 = 240 ml/min
# (0.300 / 0.240 L/min, RER = 0.80); energy 4 kcal/g CHO, 9 kcal/g FAT.
#   Frayn (1983):           CHO = 4.55*0.240 - 3.21*0.300   = 0.12900
#                           FAT = 1.67*0.300 - 1.67*0.240   = 0.10020
#   Peronnet (1991):        CHO = 4.585*0.240 - 3.2255*0.300 = 0.13275
#                           FAT = 1.6946*0.300 - 1.7012*0.240 = 0.100092
#   Jeukendrup (2005) 40-50 %: CHO = 4.344*0.240 - 3.061*0.300 = 0.12426
#                           FAT = 1.695*0.300 - 1.701*0.240  = 0.10026
# Tolerance 1e-10: exact arithmetic on decimal coefficients.
rest <- data.frame(VO2 = rep(300, 30), VCO2 = rep(240, 30),
                   RER = rep(0.8, 30), HR = rep(60, 30))

test_that("met_basal defaults to Frayn (v0.2.0 behaviour)", {
  b <- met_basal(step_time = 20, db = rest, cv_var = "VO2")
  expect_equal(b$x_CHO, 0.129, tolerance = 1e-10)
  expect_equal(b$x_FAT, 0.1002, tolerance = 1e-10)
  expect_equal(b$x_Kcal, 0.129 * 4 + 0.1002 * 9, tolerance = 1e-10)
})

test_that("met_basal uses the requested author equations", {
  b_p <- met_basal(20, rest, "VO2", author = "Peronnet")
  expect_equal(b_p$x_CHO, 0.13275, tolerance = 1e-10)
  expect_equal(b_p$x_FAT, 0.100092, tolerance = 1e-10)
  expect_equal(b_p$x_Kcal, 0.13275 * 4 + 0.100092 * 9, tolerance = 1e-10)

  # Rest = 10 % VO2max: low-intensity Jeukendrup set, extrapolated
  b_j <- met_basal(20, rest, "VO2", author = "Jeukendrup", VO2max = 3000)
  expect_equal(b_j$x_CHO, 0.12426, tolerance = 1e-10)
  expect_equal(b_j$x_FAT, 0.10026, tolerance = 1e-10)

  expect_error(met_basal(20, rest, "VO2", author = "Jeukendrup"), "VO2max is required")
})

test_that("substrate_oxidation picks the Jeukendrup set per value", {
  # 1200 / 3000 = 40 % -> 4.344*1.020 - 3.061*1.200 = 0.75768
  # 1800 / 3000 = 60 % -> 4.210*1.620 - 2.962*1.800 = 1.48860
  s <- substrate_oxidation(VO2 = c(1200, 1800), VCO2 = c(1020, 1620),
                           author = "Jeukendrup", VO2max = 3000)
  expect_equal(s$CHO, c(0.75768, 1.4886), tolerance = 1e-10)
  expect_equal(s$FAT, c(1.695 * 1.2 - 1.701 * 1.02, 1.695 * 1.8 - 1.701 * 1.62),
               tolerance = 1e-10)
})
