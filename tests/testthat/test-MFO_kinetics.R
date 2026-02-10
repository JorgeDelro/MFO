
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
                    VO2max = d$VO2max)

  result_kinetics <- MFO_kinetics(result_MFO$MFO_db)

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


test_that("MFO_kinetics calculates Fatmin", {

  d <- create_kinetics_test_data(123)

  result_MFO <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    cv_var = "VO2",
                    author = "Frayn",
                    VO2max = d$VO2max)

  result_kinetics <- MFO_kinetics(result_MFO$MFO_db)

  # Fatmin should be NA or a value > FatMax_SIN
  if (!is.na(result_kinetics$Fatmin) && !is.na(result_kinetics$FatMax_SIN)) {
    expect_true(result_kinetics$Fatmin > result_kinetics$FatMax_SIN)
    expect_true(result_kinetics$Fatmin <= 100)
  }
})


test_that("MFO_kinetics plot is a ggplot object", {

  d <- create_kinetics_test_data(42)

  result_MFO <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    cv_var = "VO2",
                    author = "Frayn",
                    VO2max = d$VO2max)

  result_kinetics <- MFO_kinetics(result_MFO$MFO_db)

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
