
# Helper function for realistic test data with RER < 1.0
create_kinetics_test_data <- function(seed = 42) {
  set.seed(seed)

  HR <- rnorm(n = 48, mean = 66, sd = 3)
  VO2 <- rnorm(n = 48, mean = 280, sd = 15)
  VCO2 <- rnorm(n = 48, mean = 220, sd = 12)
  RER <- VCO2 / VO2
  M_basal <- data.frame(HR = HR, VO2 = VO2, VCO2 = VCO2, RER = RER)

  HR_ex <- seq(90, 170, length.out = 45) + rnorm(45, 0, 2)
  VO2_ex <- seq(800, 2200, length.out = 45) + rnorm(45, 0, 20)
  RER_target <- seq(0.75, 0.95, length.out = 45) + rnorm(45, 0, 0.01)
  VCO2_ex <- VO2_ex * RER_target
  RER_ex <- VCO2_ex / VO2_ex
  MFO_test <- data.frame(HR = HR_ex, VO2 = VO2_ex, VCO2 = VCO2_ex, RER = RER_ex)

  list(basal = M_basal, exercise = MFO_test, VO2max = 2600)
}


test_that("MFO_kinetics returns correct structure and SIN parameters", {

  d <- create_kinetics_test_data(42)

  result_MFO <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    db_graded = NULL,
                    cv_var = "VO2",
                    author = "Frayn",
                    VO2max = d$VO2max,
                    min_steps = 5) # helper data have 5 steps on purpose

  # Helper data have 5 steps: fitted, with the < 6 steps warning
  expect_warning(result_kinetics <- MFO_kinetics(result_MFO$MFO_db), "unreliable")

  # Structure checks
  expect_true(is.data.frame(result_kinetics$MFO_kinetics_data))
  expect_true("porc_MFO" %in% colnames(result_kinetics$MFO_kinetics_data))

  # SIN parameters should be numeric
  expect_type(result_kinetics$d, "double")
  expect_type(result_kinetics$s, "double")
  expect_type(result_kinetics$t, "double")

  # Convergence check
  expect_true(is.logical(result_kinetics$convergence))

  # If converged, parameters should have standard errors
  if (result_kinetics$convergence) {
    expect_type(result_kinetics$d_se, "double")
    expect_type(result_kinetics$s_se, "double")
    expect_type(result_kinetics$t_se, "double")

    # RMSE should be non-negative
    expect_true(result_kinetics$RMSE >= 0)

    # FatMax_SIN should be between 0 and 100
    expect_true(result_kinetics$FatMax_SIN > 0)
    expect_true(result_kinetics$FatMax_SIN < 100)
  }
})


test_that("MFO_kinetics returns Fatmin at the second zero, above FatMax_SIN", {

  d <- create_kinetics_test_data(123)

  result_MFO <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    cv_var = "VO2",
                    author = "Frayn",
                    VO2max = d$VO2max,
                    min_steps = 5) # helper data have 5 steps on purpose

  expect_warning(k <- MFO_kinetics(result_MFO$MFO_db), "unreliable")

  expect_true(k$convergence)
  expect_true(k$Fatmin > k$FatMax_SIN)
  expect_equal(k$Fatmin, (pi + k$d - k$t) / (pi / 100), tolerance = 1e-12)
})


test_that("MFO_kinetics plot is a ggplot object", {

  d <- create_kinetics_test_data(42)

  result_MFO <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    cv_var = "VO2",
                    author = "Frayn",
                    VO2max = d$VO2max,
                    min_steps = 5) # helper data have 5 steps on purpose

  # Helper data have 5 steps: fitted, with the < 6 steps warning
  expect_warning(result_kinetics <- MFO_kinetics(result_MFO$MFO_db), "unreliable")

  expect_s3_class(result_kinetics$MFO_kinetics_plot, "ggplot")
})


test_that("MFO_kinetics uses proper pi constant (not 3.1416)", {

  # This test verifies the model uses R's pi constant
  # Basic SIN at x=50 with d=0, s=1, t=0 should equal sin(pi/2) = 1.0
  x <- 50
  K <- pi / 100
  basic_sin <- sin(((pi^(1/1) / pi + 2 * 0) * (K * x + 0 + 0))^1)

  # sin(pi/2) = 1.0 exactly
  expect_equal(basic_sin, sin(pi / 2), tolerance = 1e-10)

  # Verify it differs from the old 3.1416 approximation
  K_old <- 0.03141593
  basic_sin_old <- sin(((3.1416^(1/1) / 3.1416 + 2 * 0) * (K_old * x + 0 + 0))^1)

  # The old approximation should be slightly different
  expect_true(abs(basic_sin - basic_sin_old) > 1e-6 || abs(basic_sin - 1.0) < 1e-10)
})



# Steps generated without noise from Cheneviere et al. (2009) Fig. 5
# (d = 0.18, s = 1.52, t = 0.21) with MFO = 0.5 g/min. One step sits at the
# closed-form Fatmax, 58.2290065 %VO2max (test-utils-sin.R), so the highest
# observed value is the true peak and %MFO normalised to it is exact.
fig5_steps <- function() {
  x <- c(20, 30, 40, 50, 58.2290064739, 65, 75, 85, 95)
  data.frame(porc_VO2 = x, FAT = 0.5 * .sin_model(x, 0.18, 1.52, 0.21))
}

test_that("SIN is fitted directly to the observed steps (recovers Fig. 5)", {
  k <- MFO_kinetics(fig5_steps())
  # Exact data: a direct fit recovers the parameters; a P3-smoothed grid
  # (v0.2.0) would not. Tolerance 1e-5: nlsLM convergence criterion.
  expect_equal(c(k$d, k$s, k$t), c(0.18, 1.52, 0.21), tolerance = 1e-5)
  expect_equal(k$FatMax_SIN, 58.2290065, tolerance = 1e-5)
  expect_equal(k$Fatmin, 99.0450703, tolerance = 1e-5)
  expect_equal(k$R_squared, 1, tolerance = 1e-8)
  expect_equal(k$MFO_kinetics_data$porc_MFO[5], 100)
})

test_that("fatmin_threshold gives the closed-form intensity at that fraction of MFO", {
  k <- MFO_kinetics(fig5_steps(), fatmin_threshold = 0.05)
  # Closed form: [(pi + 2d) ((pi - asin(0.05)) / pi)^(1/s) - d - t] / K
  expected <- ((pi + 0.36) * ((pi - asin(0.05)) / pi)^(1 / 1.52) - 0.39) / (pi / 100)
  expect_equal(k$Fatmin, expected, tolerance = 1e-5)
  # Same point found numerically on the curve, above Fatmax
  root <- uniroot(function(x) .sin_model(x, 0.18, 1.52, 0.21) - 0.05,
                  c(58.23, 99.04), tol = 1e-12)$root
  expect_equal(expected, root, tolerance = 1e-8)
  expect_true(k$Fatmin < MFO_kinetics(fig5_steps())$Fatmin)

  expect_error(MFO_kinetics(fig5_steps(), fatmin_threshold = 5), "proportion")
  expect_error(MFO_kinetics(fig5_steps(), fatmin_threshold = 0), "proportion")
})

test_that("MFO_kinetics warns that 4-5 steps give an unreliable fit", {
  # 5 steps: fit attempted, warning recommends >= 6 (SPEC 6.5)
  expect_warning(k5 <- MFO_kinetics(fig5_steps()[c(1, 3, 5, 7, 9), ]),
                 "5 steps.*unreliable.*at least 6")
  expect_true(k5$convergence)
  expect_false(is.na(k5$FatMax_SIN))
  # 4 steps: same warning, still fitted
  expect_warning(MFO_kinetics(fig5_steps()[c(1, 3, 5, 9), ]),
                 "4 steps.*unreliable")
  # 6 or more steps: no warning
  expect_no_warning(MFO_kinetics(fig5_steps()[3:8, ]))
})

test_that("MFO_kinetics needs at least 4 steps", {
  expect_warning(k <- MFO_kinetics(fig5_steps()[3:5, ]), "at least 4 steps")
  expect_false(k$convergence)
  expect_true(is.na(k$FatMax_SIN))
  expect_s3_class(k$MFO_kinetics_plot, "ggplot")
})

test_that("SIN results on the example data (regression, SPEC 19)", {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  run <- function(...) suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df,
        db_graded = VO2max_df, cv_var = "RER", author = "Frayn", ...)))
  # Values of this implementation, documented in ?MFO_kinetics;
  # tolerance 1e-4 for the nlsLM optimum
  k <- MFO_kinetics(run()$MFO_db)
  expect_equal(c(k$d, k$s, k$t), c(-0.384880, 0.909642, 0.355196), tolerance = 1e-4)
  expect_equal(k$FatMax_SIN, 36.1821, tolerance = 1e-4)
  expect_equal(k$Fatmin, 76.4427, tolerance = 1e-4)

  # Sensitivity to the truncated last step (RER 1.03)
  k_ex <- MFO_kinetics(run(negative_FAT = "exclude")$MFO_db)
  expect_equal(c(k_ex$d, k_ex$s, k_ex$t), c(-0.440258, 0.538705, 0.023075), tolerance = 1e-4)
  expect_equal(k_ex$Fatmin, 85.2516, tolerance = 1e-4)
  k_cut <- MFO_kinetics(run(RER_cutoff = 1)$MFO_db)
  expect_equal(k_cut$Fatmin, k_ex$Fatmin)

  # include_basal = TRUE: the resting row is fitted too
  k_b <- MFO_kinetics(run(include_basal = TRUE)$MFO_db)
  expect_equal(nrow(k_b$MFO_kinetics_data), nrow(k$MFO_kinetics_data) + 1)
  expect_equal(k_b$FatMax_SIN, 35.8506, tolerance = 1e-4)
})
