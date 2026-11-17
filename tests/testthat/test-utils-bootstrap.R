# Residual bootstrap of the fit (SPEC 13.1). Expected values: hat values and
# residuals of lm() computed by hand, a noiseless parabola (zero residuals,
# so every replicate equals the fit) and, off CRAN, the coverage of the
# percentile interval on a parabola with independent normal errors.

.parabola_db <- function(x = seq(20, 75, length.out = 8), noise = 0) {
  # FAT = 0.4 - 0.0004 (x - 45)^2: Fatmax 45 %VO2max, MFO 0.4 g/min
  data.frame(Step = seq_along(x), Load = 25 * seq_along(x), porc_VO2 = x,
             FAT = 0.4 - 0.0004 * (x - 45)^2 + noise, CHO = 0.02 * x,
             RER = 0.8 + 0.003 * (x - 20), VO2 = 30 * x)
}

test_that("modified residuals are r / sqrt(1 - h), centred", {
  db <- .parabola_db(noise = c(0.01, -0.02, 0.015, 0, -0.01, 0.02, -0.005, 0.01))
  mod <- lm(FAT ~ porc_VO2 + I(porc_VO2^2), data = db)
  X <- model.matrix(mod)
  h <- diag(X %*% solve(t(X) %*% X) %*% t(X))
  m <- residuals(mod) / sqrt(1 - h)
  expect_equal(.modified_residuals(mod), m - mean(m))
  expect_equal(mean(.modified_residuals(mod)), 0)
  # 4 steps and a cubic: every leverage is 1
  mod4 <- lm(FAT ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3), data = db[1:4, ])
  expect_error(.modified_residuals(mod4), "more steps than model parameters")
})

test_that("a noiseless fit gives every replicate equal to the estimate", {
  db <- .parabola_db()
  reps <- .bootstrap_fit(db, "P2", 50)
  expect_equal(nrow(reps), 50)
  expect_equal(unname(reps[, "FAT_MAX"]), rep(45, 50), tolerance = 1e-8)
  expect_equal(unname(reps[, "MFO"]), rep(0.4, 50), tolerance = 1e-8)
  # Fatmax load: 45 %VO2max lies between step 4 (43.57 %, 100 W) and step 5
  # (51.43 %, 125 W), linear interpolation
  expect_equal(unname(reps[1, "FAT_MAX_load"]),
               approx(db$porc_VO2, db$Load, xout = 45)$y)
  ci <- .percentile_ci(reps, 0.95)
  expect_equal(unname(ci["FAT_MAX", ]), c(45, 45), tolerance = 1e-8)
})

test_that("MRER resamples its two regressions; MV has no bootstrap", {
  db <- .parabola_db(noise = c(0.01, -0.02, 0.015, 0, -0.01, 0.02, -0.005, 0.01))
  db$RER <- db$RER + c(0.01, -0.01, 0.005, 0, -0.005, 0.01, -0.01, 0)
  reps <- suppressWarnings(.bootstrap_fit(db, "MRER", 100))
  expect_gt(nrow(reps), 90)
  expect_gt(sd(reps[, "FAT_MAX"]), 0)
  expect_error(.bootstrap_fit(db, "MV", 10), "fits no model")
})

test_that("seed gives the same replicates and restores the random state", {
  db <- .parabola_db(noise = c(0.01, -0.02, 0.015, 0, -0.01, 0.02, -0.005, 0.01))
  a <- .with_seed(5, .bootstrap_fit(db, "P2", 30))
  b <- .with_seed(5, .bootstrap_fit(db, "P2", 30))
  expect_identical(a, b)
  set.seed(1); x1 <- runif(1)
  set.seed(1); invisible(.with_seed(5, runif(3))); x2 <- runif(1)
  expect_identical(x1, x2)
})

test_that("coverage of the percentile interval in the ideal case (parabola, normal errors)", {
  skip_on_cran()
  # Ideal case: correct model, independent N(0, 0.02) errors, 8 steps. The
  # percentile interval of the vertex under-covers: 0.873 with 150 tests and
  # B = 300 (seed 51). Here 100 tests and B = 200 (binomial SE about 0.034)
  # check that coverage is not far below that; the full study, with the
  # generating curve as truth, is in inst/validation/bootstrap_coverage.R.
  set.seed(51)
  hits <- replicate(100, {
    db <- .parabola_db(noise = rnorm(8, 0, 0.02))
    ci <- .percentile_ci(.bootstrap_fit(db, "P2", 200), 0.95)
    ci["FAT_MAX", 1] <= 45 && 45 <= ci["FAT_MAX", 2]
  })
  expect_gte(mean(hits), 0.78)
  expect_lte(mean(hits), 0.99)
})
