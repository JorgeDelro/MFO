# VO2max_from_test() (SPEC 14). Expected values from the example graded test
# (VO2max_df, 20-s records; the 30-s window is rounded up to 2 records):
# computed by hand in each test from the same table, then compared exactly.

.graded <- function() {
  data(VO2max_df, package = "MFO", envir = environment())
  data.frame(VO2max_df)
}
.v <- function(...) suppressMessages(VO2max_from_test(.graded(), step_time = 20, ...))
.roll2 <- function(x) max(stats::filter(x, c(0.5, 0.5), sides = 1), na.rm = TRUE)

test_that("VO2max is the highest 40-s mean, as in MFO()", {
  g <- .graded()
  v <- .v(age = 30)
  expect_s3_class(v, "MFO_VO2max")
  expect_equal(v$VO2max, .roll2(g$VO2))
  expect_equal(v$VO2max, 3315)
  expect_equal(v$window, 40)
  expect_equal(v$window_requested, 30)
})

test_that("criteria are computed from the last stages, peak RER and peak HR", {
  g <- .graded()
  v <- .v(age = 30)
  # plateau: last 2 records of the last Load stage minus those of the previous one
  stage <- cumsum(c(TRUE, diff(g$Load) != 0))
  last <- max(stage)
  m <- function(k) mean(utils::tail(g$VO2[stage == k], 2))
  cr <- v$criteria
  expect_equal(cr$value[cr$criterion == "plateau"], m(last) - m(last - 1))
  expect_equal(cr$value[cr$criterion == "RER"], .roll2(g$RER))
  # Tanaka: 208 - 0.7 * 30 = 187 bpm
  expect_equal(v$HRmax_predicted, 187)
  expect_equal(cr$value[cr$criterion == "HR"], .roll2(g$HR) / 187)
  expect_equal(.v(age = 30, HRmax_formula = "fox")$HRmax_predicted, 190)
})

test_that("the label follows the plateau, or RER and HR together", {
  # Example: plateau not met (+247 ml/min), RER and HR met at age 30
  expect_equal(.v(age = 30)$type, "VO2max")
  expect_equal(.v(age = 30, RER_threshold = 1.2)$type, "VO2peak")
  expect_equal(.v(age = 30, HR_threshold = 1)$type, "VO2peak")
  # a looser plateau threshold labels VO2max without RER/HR
  expect_equal(suppressWarnings(.v(plateau_threshold = 300))$type, "VO2max")
})

test_that("without age the HR criterion is not evaluated, with a warning", {
  expect_warning(v <- .v(), "age not given")
  expect_true(is.na(v$criteria$met[v$criteria$criterion == "HR"]))
  expect_equal(v$type, "VO2peak")
  expect_error(.v(age = -1), "positive")
  expect_output(print(.v(age = 30)), "VO2max = 3315.0 ml/min")
  expect_output(print(v), "HR +not evaluated")
})

test_that("without Load the plateau is not evaluated", {
  g <- .graded()
  g$Load <- NULL
  v <- suppressMessages(VO2max_from_test(g, 20, age = 30))
  expect_true(is.na(v$criteria$value[v$criteria$criterion == "plateau"]))
  expect_equal(v$type, "VO2max")
})

test_that("MFO() reports the type and accepts a VO2max_from_test() result", {
  data(list = c("basal_df", "MFO_df"), package = "MFO", envir = environment())
  run <- function(...) suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df, cv_var = "RER",
        author = "Frayn", energy_equivalents = "classic", ...)))
  r <- run(db_graded = .graded(), age = 30)
  expect_equal(r$VO2max_type, "VO2max")
  expect_equal(r$VO2max_criteria, .v(age = 30)$criteria)
  v <- .v(age = 30, RER_threshold = 1.2)
  r2 <- run(VO2max = v)
  expect_equal(r2$VO2max_type, "VO2peak")
  expect_equal(r2$VO2max, r$VO2max)
  expect_equal(r2$VO2max_window, 40)
  expect_equal(r2$MFO, r$MFO)
  # a plain number has no type
  expect_true(is.na(run(VO2max = 3315)$VO2max_type))
})

test_that("MFO() without age gives a once-per-session message, not a warning", {
  old <- .mfo_state$age_message_shown
  withr::defer(.mfo_state$age_message_shown <- old)
  .mfo_state$age_message_shown <- NULL
  expect_message(.age_default_message(), "age not given")
  expect_no_message(.age_default_message())
})
