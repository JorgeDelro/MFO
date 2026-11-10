# Hand calculations: constant resting data VO2 = 300, VCO2 = 240 ml/min
# (0.300 / 0.240 L/min, RER = 0.80); energy 4 kcal/g CHO, 9 kcal/g FAT.
#   Frayn (1983):           CHO = 4.55*0.240 - 3.21*0.300   = 0.12900
#                           FAT = 1.67*0.300 - 1.67*0.240   = 0.10020
#   Peronnet (1991):        CHO = 4.585*0.240 - 3.2255*0.300 = 0.13275
#                           FAT = 1.6946*0.300 - 1.7012*0.240 = 0.100092
#   Jeukendrup (2005) at rest: 100 % glucose pair from Table 4 constants
#     (glucose 0.7455/0.7426, fatty acid 2.0092/1.4136 L/g),
#     det = 2.0092*0.7426 - 0.7455*1.4136 = 0.43819
#     CHO = (2.0092*0.240 - 1.4136*0.300) / det = 0.1326538
#     FAT = (0.7426*0.300 - 0.7455*0.240) / det = 0.1000929
# Tolerance 1e-10 for the decimal coefficients; 1e-6 for the derived ones
# (hand values rounded to 7 decimals).
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

  # Jeukendrup at rest: 100 % glucose pair, no VO2max needed
  b_j <- met_basal(20, rest, "VO2", author = "Jeukendrup")
  expect_equal(b_j$x_CHO, 0.1326538, tolerance = 1e-6)
  expect_equal(b_j$x_FAT, 0.1000929, tolerance = 1e-6)
})

test_that("substrate_oxidation picks the Jeukendrup set per value", {
  # 1200 / 3000 = 40 % -> 50/50 mix: O2 0.7869, CO2 0.78385 L/g, det = 0.46255
  #   CHO = (2.0092*1.020 - 1.4136*1.200) / det = 0.7632998
  #   FAT = (0.78385*1.200 - 0.7869*1.020) / det = 0.2983075
  # 1800 / 3000 = 60 % -> 20/80 mix: O2 0.81174, CO2 0.8086 L/g, det = 0.47716
  #   CHO = (2.0092*1.620 - 1.4136*1.800) / det = 1.4888483
  #   FAT = (0.8086*1.800 - 0.81174*1.620) / det = 0.2943671
  s <- substrate_oxidation(VO2 = c(1200, 1800), VCO2 = c(1020, 1620),
                           author = "Jeukendrup", VO2max = 3000)
  expect_equal(s$CHO, c(0.7632998, 1.4888483), tolerance = 1e-6)
  expect_equal(s$FAT, c(0.2983075, 0.2943671), tolerance = 1e-6)

  expect_error(substrate_oxidation(1200, 1020, "Jeukendrup"), "VO2max is required")
})
