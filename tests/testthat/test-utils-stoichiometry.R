# Cross-check: coefficients derived from the Table 4 constants of Jeukendrup &
# Wallis (2005, Int J Sports Med 26 Suppl 1) against those printed in their
# Table 5, which the package uses for exercise. The article rounds them, so
# the comparison uses an absolute tolerance of 0.006 (largest gap: 50/50
# cho_VO2, derived 3.0561 vs printed 3.061).
expect_close <- function(object, expected, tol = 0.006) {
  expect_true(all(abs(object - expected) < tol),
              info = paste(round(object, 5), collapse = " / "))
}

test_that("50/50 glucose/glycogen reproduces the 40-50 % VO2max set", {
  k <- .jeukendrup_coefficients(0.5)
  expect_close(k[c("cho_VCO2", "cho_VO2")], c(4.344, 3.061))
  expect_close(k[c("fat_VO2", "fat_VCO2")], c(1.695, 1.701))
})

test_that("20/80 glucose/glycogen reproduces the 50-75 % VO2max set", {
  k <- .jeukendrup_coefficients(0.2)
  expect_close(k[c("cho_VCO2", "cho_VO2")], c(4.210, 2.962))
  expect_close(k[c("fat_VO2", "fat_VCO2")], c(1.695, 1.701))
})

test_that("100/0 (resting pair, package decision) gives 4.585 / 3.226", {
  k <- .jeukendrup_coefficients(1)
  expect_close(k[c("cho_VCO2", "cho_VO2")], c(4.585, 3.226))
  expect_close(k[c("fat_VO2", "fat_VCO2")], c(1.695, 1.701))
})

test_that("Peronnet coefficients are derived from their Table 1 (SPEC 19)", {
  # Expected values from SPEC 19 (finding 6), solving the 2x2 system by hand;
  # tolerance 5e-6 = half a unit in the 5th decimal
  expect_close(.peronnet_coefficients(), c(4.58519, 3.22597, 1.69469, 1.70130),
               tol = 5e-6)
  s <- substrate_oxidation(VO2 = 1000, VCO2 = 850, author = "Peronnet")
  expect_equal(s$CHO, 4.585193 * 0.85 - 3.225975 * 1, tolerance = 1e-6)
  expect_equal(s$FAT, 1.694687 * 1 - 1.701305 * 0.85, tolerance = 1e-6)
})

test_that("Frayn's printed equations agree with his Table 1 (cross-check)", {
  # Frayn (1983, J Appl Physiol 55: 628-634) Table 1: glucose 0.746 L O2 and
  # 0.746 L CO2 per g; fat 2.03 L O2 and 1.43 L CO2 per g. Derived by hand:
  # det = 2.03 * 0.746 - 0.746 * 1.43 = 0.4476; CHO = 4.5353 VCO2 - 3.1948 VO2;
  # FAT = 1.6667 VO2 - 1.6667 VCO2. Printed (used): 4.55 / 3.21 / 1.67 / 1.67.
  # Tolerance 0.02: the printed CHO pair differs by 0.015.
  k <- .derive_oxidation_coefficients(0.746, 0.746, 2.03, 1.43)
  expect_close(k, c(4.5353, 3.1948, 1.6667, 1.6667), tol = 1e-4)
  expect_close(k, c(4.55, 3.21, 1.67, 1.67), tol = 0.02)
  s <- substrate_oxidation(VO2 = 1000, VCO2 = 850, author = "Frayn")
  expect_equal(c(s$CHO, s$FAT), c(4.55 * 0.85 - 3.21, 1.67 - 1.67 * 0.85))
})

test_that("the 2x2 solver matches the Peronnet & Massicotte derivation", {
  # Same glucose/fatty-acid constants as Peronnet & Massicotte (1991, Table 1);
  # SPEC 19 lists the derived values 4.58519 / 3.22597 / 1.69469 / 1.70130.
  k <- .derive_oxidation_coefficients(0.7455, 0.7426, 2.0092, 1.4136)
  expect_close(k, c(4.58519, 3.22597, 1.69469, 1.70130), tol = 5e-6)
})
