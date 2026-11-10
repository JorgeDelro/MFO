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

test_that("the 2x2 solver matches the Peronnet & Massicotte derivation", {
  # Same glucose/fatty-acid constants as Peronnet & Massicotte (1991, Table 1);
  # SPEC 19 lists the derived values 4.58519 / 3.22597 / 1.69469 / 1.70130.
  k <- .derive_oxidation_coefficients(0.7455, 0.7426, 2.0092, 1.4136)
  expect_close(k, c(4.58519, 3.22597, 1.69469, 1.70130), tol = 5e-6)
})
