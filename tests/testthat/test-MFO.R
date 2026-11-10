
# Helper function to create realistic test data
# VCO2 starts well below VO2 (RER ~0.75) and rises to approach VO2 (RER ~0.95)
# This produces the classic inverted-U fat oxidation curve
create_test_data <- function(seed = 42, n_exercise = 45) {
  set.seed(seed)

  # Basal: 48 rows (16 min at 20s intervals)
  HR <- rnorm(n = 48, mean = 66, sd = 3)
  VO2 <- rnorm(n = 48, mean = 280, sd = 15)
  VCO2 <- rnorm(n = 48, mean = 220, sd = 12)
  RER <- VCO2 / VO2
  M_basal <- data.frame(HR = HR, VO2 = VO2, VCO2 = VCO2, RER = RER)

  # Exercise: n rows with realistic RER progression (0.75 -> 0.95)
  HR_ex <- seq(90, 170, length.out = n_exercise) + rnorm(n_exercise, 0, 2)
  VO2_ex <- seq(800, 2200, length.out = n_exercise) + rnorm(n_exercise, 0, 20)
  # VCO2 starts at ~75% of VO2 and rises to ~95%, keeping RER < 1.0
  RER_target <- seq(0.75, 0.95, length.out = n_exercise) + rnorm(n_exercise, 0, 0.01)
  VCO2_ex <- VO2_ex * RER_target
  RER_ex <- VCO2_ex / VO2_ex
  MFO_test <- data.frame(HR = HR_ex, VO2 = VO2_ex, VCO2 = VCO2_ex, RER = RER_ex)

  list(basal = M_basal, exercise = MFO_test, VO2max = 2600)
}


test_that("MFO function returns correct structure with P3 method", {

  d <- create_test_data(42)

  result <- MFO(step_time = 20,
                db_MFO = d$exercise,
                db_basal = d$basal,
                db_graded = NULL,
                cv_var = "VO2",
                author = "Frayn",
                VO2max = d$VO2max,
                method = "P3")

  # Structure checks
  expect_true(is.data.frame(result$MFO_db))
  expect_true("porc_VO2" %in% colnames(result$MFO_db))
  expect_true("FAT" %in% colnames(result$MFO_db))
  expect_true("CHO" %in% colnames(result$MFO_db))
  expect_true("HR" %in% colnames(result$MFO_db))
  expect_true("RER" %in% colnames(result$MFO_db))

  # Numeric checks
  expect_type(result$MFO, "double")
  expect_type(result$FAT_MAX, "double")
  expect_type(result$R_squared, "double")
  expect_type(result$RMSE, "double")
  expect_type(result$x_CHO, "double")
  expect_type(result$x_FAT, "double")
  expect_type(result$x_Kcal, "double")

  # MFO should be non-negative
  expect_true(result$MFO >= 0)

  # FatMax should be within reasonable range
  expect_true(result$FAT_MAX > 0)
  expect_true(result$FAT_MAX < 100)

  # R-squared should be between 0 and 1 (with realistic data)
  expect_true(result$R_squared >= 0)
  expect_true(result$R_squared <= 1)

  # Method should be recorded
  expect_equal(result$method, "P3")

  # FatMax zone should exist
  expect_true(is.numeric(result$FatMax_zone_lower) || is.na(result$FatMax_zone_lower))
  expect_true(is.numeric(result$FatMax_zone_upper) || is.na(result$FatMax_zone_upper))

  # Crossover point check
  expect_true(is.numeric(result$crossover_point) || is.na(result$crossover_point))
})


test_that("MFO function works with P2 method", {

  d <- create_test_data(42)

  result <- MFO(step_time = 20,
                db_MFO = d$exercise,
                db_basal = d$basal,
                db_graded = NULL,
                cv_var = "VO2",
                author = "Frayn",
                VO2max = d$VO2max,
                method = "P2")

  expect_equal(result$method, "P2")
  expect_type(result$MFO, "double")
  expect_true(result$MFO >= 0)
})


test_that("MFO function works with MRER method", {

  d <- create_test_data(42)

  result <- MFO(step_time = 20,
                db_MFO = d$exercise,
                db_basal = d$basal,
                db_graded = NULL,
                cv_var = "VO2",
                author = "Frayn",
                VO2max = d$VO2max,
                method = "MRER")

  expect_equal(result$method, "MRER")
  expect_type(result$MFO, "double")
  expect_true(result$MFO >= 0)
})


test_that("MFO function works with Jeukendrup and Peronnet equations", {

  d <- create_test_data(42)

  # Jeukendrup: steps at 39.9 and 83.2 %VO2max are outside 40-75 %
  expect_warning(
    result_j <- MFO(step_time = 20,
                    db_MFO = d$exercise,
                    db_basal = d$basal,
                    cv_var = "VO2",
                    author = "Jeukendrup",
                    VO2max = d$VO2max),
    "2 step\\(s\\) outside this range")

  expect_type(result_j$MFO, "double")

  # Peronnet
  result_p <- MFO(step_time = 20,
                  db_MFO = d$exercise,
                  db_basal = d$basal,
                  cv_var = "VO2",
                  author = "Peronnet",
                  VO2max = d$VO2max)

  expect_type(result_p$MFO, "double")
})


test_that("MFO function calculates relative MFO and HR-based FatMax", {

  d <- create_test_data(42)

  result <- MFO(step_time = 20,
                db_MFO = d$exercise,
                db_basal = d$basal,
                cv_var = "VO2",
                author = "Frayn",
                VO2max = d$VO2max,
                body_mass = 75,
                fat_free_mass = 60,
                HRmax = 190)

  # Relative MFO should be non-negative
  expect_true(!is.na(result$MFO_relative))
  expect_true(result$MFO_relative >= 0)

  # FFM-relative MFO should be non-negative
  expect_true(!is.na(result$MFO_FFM))
  expect_true(result$MFO_FFM >= 0)

  # If MFO > 0, FFM relative should be larger than body mass relative
  if (result$MFO > 0) {
    expect_true(result$MFO_FFM > result$MFO_relative)
  }

  # HR at FatMax
  expect_true(!is.na(result$FAT_MAX_HR))

  # %HRmax
  expect_true(!is.na(result$FAT_MAX_porcHRmax))
  expect_true(result$FAT_MAX_porcHRmax > 0)
  expect_true(result$FAT_MAX_porcHRmax < 100)
})


test_that("MFO function handles RER cutoff correctly", {

  d <- create_test_data(42)

  # Without cutoff
  result_no_cutoff <- MFO(step_time = 20,
                          db_MFO = d$exercise,
                          db_basal = d$basal,
                          cv_var = "VO2",
                          author = "Frayn",
                          VO2max = d$VO2max,
                          RER_cutoff = NULL)

  # With strict cutoff
  result_cutoff <- MFO(step_time = 20,
                       db_MFO = d$exercise,
                       db_basal = d$basal,
                       cv_var = "VO2",
                       author = "Frayn",
                       VO2max = d$VO2max,
                       RER_cutoff = 0.85)

  # With cutoff should have fewer or equal rows
  expect_true(nrow(result_cutoff$MFO_db) <= nrow(result_no_cutoff$MFO_db))
})


test_that("MFO function errors without VO2max", {

  d <- create_test_data(42)

  expect_error(
    MFO(step_time = 20,
        db_MFO = d$exercise,
        db_basal = d$basal,
        db_graded = NULL,
        cv_var = "VO2",
        author = "Frayn",
        VO2max = NULL),
    "You must provide a VO2max"
  )
})


test_that("substrate_oxidation returns correct values for known inputs", {

  # Test Frayn equations with known values
  # VO2 = 2000 ml/min = 2.0 L/min, VCO2 = 1700 ml/min = 1.7 L/min
  result <- substrate_oxidation(VO2 = 2000, VCO2 = 1700, author = "Frayn")

  # CHO = 4.55 * 1.7 - 3.21 * 2.0 = 7.735 - 6.42 = 1.315 g/min
  expect_equal(result$CHO, 4.55 * 1.7 - 3.21 * 2.0, tolerance = 0.001)

  # FAT = 1.67 * 2.0 - 1.67 * 1.7 = 3.34 - 2.839 = 0.501 g/min
  expect_equal(result$FAT, 1.67 * 2.0 - 1.67 * 1.7, tolerance = 0.001)

  # Test Peronnet equations
  result_p <- substrate_oxidation(VO2 = 2000, VCO2 = 1700, author = "Peronnet")
  expect_equal(result_p$FAT, 1.6946 * 2.0 - 1.7012 * 1.7, tolerance = 0.001)
})


test_that("fat_oxidation_MRER returns correct values", {

  # VO2 = 1500 ml/min, RER = 0.85
  # FAT = 1.7 * (1 - 0.85) * 1500 / 1000 = 1.7 * 0.15 * 1.5 = 0.3825 g/min
  result <- fat_oxidation_MRER(VO2 = 1500, RER = 0.85)
  expect_equal(result, 1.7 * 0.15 * 1500 / 1000, tolerance = 0.001)

  # At RER = 1.0, fat oxidation should be 0
  result_rer1 <- fat_oxidation_MRER(VO2 = 1500, RER = 1.0)
  expect_equal(result_rer1, 0)

  # At RER = 0.7, fat oxidation should be maximal
  result_rer07 <- fat_oxidation_MRER(VO2 = 1500, RER = 0.7)
  expect_true(result_rer07 > result)
})


test_that("MFO function works with different step_time values", {

  set.seed(42)

  # Basal: enough rows
  HR <- rnorm(n = 90, mean = 66, sd = 3)
  VO2 <- rnorm(n = 90, mean = 280, sd = 15)
  VCO2 <- rnorm(n = 90, mean = 220, sd = 12)
  RER <- VCO2 / VO2
  M_basal <- data.frame(HR = HR, VO2 = VO2, VCO2 = VCO2, RER = RER)

  # 5 steps x 18 records (3 min at 10s intervals)
  n <- 90
  HR_ex <- seq(90, 170, length.out = n) + rnorm(n, 0, 2)
  VO2_ex <- seq(800, 2200, length.out = n) + rnorm(n, 0, 20)
  RER_target <- seq(0.75, 0.95, length.out = n) + rnorm(n, 0, 0.01)
  VCO2_ex <- VO2_ex * RER_target
  RER_ex <- VCO2_ex / VO2_ex
  MFO_test <- data.frame(HR = HR_ex, VO2 = VO2_ex, VCO2 = VCO2_ex, RER = RER_ex)

  result <- MFO(step_time = 10,
                db_MFO = MFO_test,
                db_basal = M_basal,
                cv_var = "VO2",
                author = "Frayn",
                VO2max = 2600,
                step_duration = 180,
                steady_state_duration = 60)

  expect_type(result$MFO, "double")
  expect_true(result$MFO >= 0)
})


test_that("MFO uses the rolling-mean VO2max from db_graded and returns it", {
  d <- create_test_data(42)
  # Hand calculation: pairwise means 2000, 3050, 3000, 3200, 2750 -> 3200 ml/min
  graded <- data.frame(VO2 = c(1000, 3000, 3100, 2900, 3500, 2000))

  expect_message(
    result <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = d$basal,
                  db_graded = graded, cv_var = "VO2", author = "Frayn"),
    "40 s")

  expect_equal(result$VO2max, 3200)
  expect_equal(result$VO2max_window, 40)
  expect_equal(result$VO2max_window_requested, 30)
  expect_equal(result$MFO_db$porc_VO2, result$MFO_db$VO2 * 100 / 3200)

  # Same window as step_time reproduces the v0.2.0 instantaneous maximum
  result_inst <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = d$basal,
                     db_graded = graded, cv_var = "VO2", author = "Frayn",
                     vo2max_window = 20)
  expect_equal(result_inst$VO2max, 3500)

  # P3 is invariant to rescaling the x axis: MFO is unchanged and Fatmax
  # scales by the VO2max ratio (3500 / 3200)
  expect_equal(result$MFO, result_inst$MFO)
  expect_equal(result$FAT_MAX, result_inst$FAT_MAX * 3500 / 3200)
})


test_that("a supplied VO2max wins over db_graded with a warning", {
  d <- create_test_data(42)
  graded <- data.frame(VO2 = c(1000, 3000, 3100, 2900, 3500, 2000))

  expect_warning(
    result <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = d$basal,
                  db_graded = graded, VO2max = 2600, cv_var = "VO2",
                  author = "Frayn"),
    "ignoring db_graded")
  expect_equal(result$VO2max, 2600)
  expect_true(is.na(result$VO2max_window))
  expect_true(is.na(result$VO2max_window_requested))
})


test_that("MFO resting values use the same author equations as the test", {
  d <- create_test_data(42)
  # Constant rest: VO2 = 300, VCO2 = 240 ml/min; hand values in test-utils.R
  rest <- data.frame(VO2 = rep(300, 48), VCO2 = rep(240, 48),
                     RER = rep(0.8, 48), HR = rep(60, 48))

  r_p <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = rest,
             cv_var = "VO2", author = "Peronnet", VO2max = d$VO2max)
  expect_equal(r_p$x_CHO, 0.13275, tolerance = 1e-10)
  expect_equal(r_p$x_FAT, 0.100092, tolerance = 1e-10)

  r_j <- suppressWarnings(  # exercise steps outside 40-75 %, tested in test-utils.R
    MFO(step_time = 20, db_MFO = d$exercise, db_basal = rest,
        cv_var = "VO2", author = "Jeukendrup", VO2max = d$VO2max))
  expect_equal(r_j$x_CHO, 0.1326538, tolerance = 1e-6)
  expect_equal(r_j$x_FAT, 0.1000929, tolerance = 1e-6)

  r_f <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = rest,
             cv_var = "VO2", author = "Frayn", VO2max = d$VO2max)
  expect_equal(r_f$x_FAT, 0.1002, tolerance = 1e-10)

  # The resting row is not part of the fit, so MFO does not depend on it
  r_f2 <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = d$basal,
              cv_var = "VO2", author = "Frayn", VO2max = d$VO2max)
  expect_equal(r_f$MFO, r_f2$MFO)
})


test_that("include_basal = FALSE reproduces v0.2.0 on the example data", {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df,
        db_graded = VO2max_df, cv_var = "RER", author = "Frayn")))
  # Expected values: output of main (commit 9b34ceb) on the same call; the
  # change only makes the resting-row exclusion explicit (SPEC 9.5)
  expect_equal(r$MFO, 0.316712001016, tolerance = 1e-10)
  expect_equal(r$FAT_MAX, 36.170380668222, tolerance = 1e-10)
  expect_equal(r$FatMax_zone_lower, 24.190646151430, tolerance = 1e-10)
  expect_equal(r$FatMax_zone_upper, 48.340688602127, tolerance = 1e-10)
  expect_equal(r$crossover_point, 24.848177391000, tolerance = 1e-10)
  expect_equal(nrow(r$MFO_db), 8)
  expect_false("basal" %in% r$MFO_db$Load)
})


test_that("include_basal = TRUE adds the resting row as step 0 of the fit", {
  d <- create_test_data(42)
  # Constant rest: VO2 = 300, VCO2 = 240 ml/min
  # Frayn: FAT = 1.67 * 0.300 - 1.67 * 0.240 = 0.1002 g/min
  # porc_VO2 = 300 / 2600 * 100 = 11.538462 %VO2max
  rest <- data.frame(VO2 = rep(300, 48), VCO2 = rep(240, 48),
                     RER = rep(0.8, 48), HR = rep(60, 48))

  r_off <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = rest,
               cv_var = "VO2", author = "Frayn", VO2max = d$VO2max)
  r_on <- MFO(step_time = 20, db_MFO = d$exercise, db_basal = rest,
              cv_var = "VO2", author = "Frayn", VO2max = d$VO2max,
              include_basal = TRUE)

  # The resting row is kept although its HR is NA
  expect_equal(nrow(r_on$MFO_db), nrow(r_off$MFO_db) + 1)
  expect_equal(r_on$MFO_db$Load[1], "basal")
  expect_true(is.na(r_on$MFO_db$HR[1]))
  expect_equal(r_on$MFO_db$FAT[1], 0.1002, tolerance = 1e-10)
  expect_equal(r_on$MFO_db$porc_VO2[1], 300 / 2600 * 100, tolerance = 1e-10)
  expect_equal(r_on$MFO_db[-1, ], r_off$MFO_db, ignore_attr = TRUE)

  # The P3 changes, and equals a cubic fitted by hand to rest + steps
  expect_false(isTRUE(all.equal(r_on$FAT_MAX, r_off$FAT_MAX)))
  pts <- rbind(data.frame(x = 300 / 2600 * 100, y = 0.1002),
               data.frame(x = r_off$MFO_db$porc_VO2, y = r_off$MFO_db$FAT))
  cf <- coef(lm(y ~ x + I(x^2) + I(x^3), data = pts))
  p3 <- function(x) cf[1] + cf[2] * x + cf[3] * x^2 + cf[4] * x^3
  opt <- optimize(p3, range(pts$x), maximum = TRUE)
  expect_equal(r_on$FAT_MAX, opt$maximum, tolerance = 1e-4)
  expect_equal(r_on$MFO, unname(opt$objective), tolerance = 1e-8)
})
