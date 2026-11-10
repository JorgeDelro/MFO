# SIN model, Cheneviere et al. (2009) eq. [14]:
#   %MFO = sin([pi^(1/s) / (pi + 2d) * (K x + d + t)]^s),  K = pi / 100
# Characteristic points derived from [14] (SPEC 6.5):
#   first zero x1 = -(d + t) / K
#   Fatmax       = [(pi + 2d) * 2^(-1/s) - d - t] / K
#   Fatmin (x2)  = (pi + d - t) / K
# Tolerance 1e-6 (%VO2max): numerical optimum/root vs the closed forms.

K <- pi / 100

test_that("basic SIN (d = t = 0, s = 1) has zeros at 0 and 100 and its peak at 50", {
  expect_equal(.sin_model(0, 0, 1, 0), 0, tolerance = 1e-12)
  expect_equal(.sin_model(100, 0, 1, 0), 0, tolerance = 1e-12)
  expect_equal(.sin_model(50, 0, 1, 0), 1, tolerance = 1e-12)
  opt <- optimize(.sin_model, c(0, 100), d = 0, s = 1, t = 0,
                  maximum = TRUE, tol = 1e-10)
  expect_equal(opt$maximum, 50, tolerance = 1e-6)
})

test_that("SIN with Cheneviere Fig. 5 parameters matches the closed forms", {
  # Trained subject, Cheneviere et al. (2009) Fig. 5: d = 0.18, s = 1.52, t = 0.21
  d <- 0.18; s <- 1.52; t <- 0.21
  # By hand: Fatmax = [(pi + 0.36) * 2^(-1/1.52) - 0.39] / K = 58.2290065
  #          Fatmin = (pi + 0.18 - 0.21) / K                 = 99.0450703
  #          x1     = -0.39 / K                              = -12.4140856
  fatmax <- ((pi + 2 * d) * 2^(-1 / s) - d - t) / K
  fatmin <- (pi + d - t) / K
  expect_equal(fatmax, 58.2290065, tolerance = 1e-6)
  expect_equal(fatmin, 99.0450703, tolerance = 1e-6)

  opt <- optimize(.sin_model, c(0, 100), d = d, s = s, t = t,
                  maximum = TRUE, tol = 1e-10)
  expect_equal(opt$maximum, fatmax, tolerance = 1e-6)
  expect_equal(opt$objective, 1, tolerance = 1e-12)

  zero2 <- uniroot(.sin_model, c(fatmax, 100), d = d, s = s, t = t,
                   tol = 1e-12)$root
  expect_equal(zero2, fatmin, tolerance = 1e-6)
  # Just above x1 (at x1 rounding can make the base -1e-17 and the power NaN)
  expect_equal(.sin_model(-(d + t) / K + 1e-9, d, s, t), 0, tolerance = 1e-6)
})

test_that("SIN no longer uses the v0.2.0 denominator (pi^(1/s)/pi + 2d)", {
  # v0.2.0 put the Fig. 5 peak at 28.9 %VO2max instead of 58.2
  d <- 0.18; s <- 1.52; t <- 0.21
  old <- function(x) sin(((pi^(1 / s) / pi + 2 * d) * (K * x + d + t))^s)
  expect_false(isTRUE(all.equal(.sin_model(58.2290065, d, s, t),
                                old(58.2290065))))
  # With d = 0 both forms coincide
  expect_equal(.sin_model(37, 0, 1.3, 0.1),
               sin((pi^(1 / 1.3) / pi * (K * 37 + 0.1))^1.3), tolerance = 1e-12)
})
