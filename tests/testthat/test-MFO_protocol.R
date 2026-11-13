# MFO_protocol() and its use in MFO()/MFOs() (SPEC 14). Expected values come
# from the example data (72 records of 20 s = 8 steps of 180 s) and from
# MFO() with the v0.2.x arguments on the same call; comparisons are exact.

.example <- function() {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  list(basal = data.frame(basal_df), MFO = data.frame(MFO_df),
       graded = data.frame(VO2max_df))
}

# Step 8 of the example data has FAT < 0: that known warning is muffled so
# any other warning still reaches the test
.mfo <- function(...) {
  e <- .example()
  withCallingHandlers(
    suppressMessages(MFO(step_time = 20, db_MFO = e$MFO, db_basal = e$basal,
                         db_graded = e$graded, cv_var = "RER", author = "Frayn",
                         energy_equivalents = "classic", ...)),
    warning = function(w) {
      if (grepl("negative fat oxidation", conditionMessage(w))) invokeRestart("muffleWarning")
    })
}

test_that("MFO_protocol() defaults describe the classic 3-min protocol", {
  p <- MFO_protocol()
  expect_s3_class(p, "MFO_protocol")
  expect_equal(p$type, "step")
  expect_equal(p$step_duration, 180)
  expect_equal(p$steady_state_duration, 60)
  expect_equal(p$load_unit, "W")
  expect_equal(p$mode, "cycling")
  expect_null(p$load_start)
  expect_output(print(p), "<MFO_protocol> step")
  expect_output(print(MFO_protocol("step_variable", step_duration = c(300, 180))),
                "300, 180 s \\(last one repeated\\)")
})

test_that("MFO_protocol() rejects inconsistent descriptions", {
  expect_error(MFO_protocol(step_duration = c(300, 180)), "step_variable")
  expect_error(MFO_protocol(steady_state_duration = 200), "cannot be longer")
  expect_error(MFO_protocol(step_duration = -1), "positive")
  expect_error(MFO_protocol("step_variable", step_duration = c(300, 180, 180),
                            steady_state_duration = c(60, 60)), "same length")
  expect_error(MFO_protocol(load_start = 25), "both")
  expect_error(MFO_protocol("relative", load_unit = "W"), "W/kg")
  expect_equal(MFO_protocol("relative")$load_unit, "W/kg")
  expect_error(MFO_protocol("ramp"))
})

test_that(".step_bounds() cuts equal and variable steps by records", {
  b <- .step_bounds(MFO_protocol(), n_records = 72, step_time = 20)
  expect_equal(b$end, seq(9, 72, by = 9))
  expect_true(all(b$complete))
  expect_equal(b$steady_records, rep(3, 8))
  # 5-min first step (15 records), then 3-min steps (9 records), last repeated
  b <- .step_bounds(MFO_protocol("step_variable", step_duration = c(300, 180)),
                    n_records = 72, step_time = 20)
  expect_equal(b$end, c(15, 24, 33, 42, 51, 60, 69, 72))
  expect_equal(b$complete, c(rep(TRUE, 7), FALSE))
  expect_equal(b$available[8], 3)
})

test_that("protocol = NULL and an explicit step protocol give identical results", {
  r_old <- .mfo()
  r_new <- .mfo(protocol = MFO_protocol("step", 180, 60))
  r_old$MFO_plot <- r_new$MFO_plot <- NULL
  expect_identical(r_old, r_new)
  expect_s3_class(r_old$protocol, "MFO_protocol")
  expect_equal(r_old$mode, "cycling")
})

test_that("supplying protocol and step_duration warns and uses protocol", {
  expect_warning(r <- .mfo(protocol = MFO_protocol(), step_duration = 120),
                 "using protocol")
  expect_equal(nrow(r$MFO_db), 8)
})

test_that("step_variable averages the last segment of each variable step", {
  # A 6-min first step covers the first two 3-min stages, so its last minute is
  # the last minute of stage 2: the 7 steps equal stages 2 to 8 exactly
  r8 <- .mfo()
  r7 <- .mfo(protocol = MFO_protocol("step_variable", step_duration = c(360, 180)))
  expect_equal(r7$MFO_db$Step, 1:7)
  cols <- c("VO2", "VCO2", "HR", "RER", "CHO", "FAT", "Kcal", "porc_VO2")
  expect_equal(r7$MFO_db[, cols], r8$MFO_db[2:8, cols], ignore_attr = TRUE)
  # Equal durations through step_variable reproduce the step protocol
  rv <- .mfo(protocol = MFO_protocol("step_variable", step_duration = c(180, 180)))
  expect_equal(rv$MFO, r8$MFO)
  expect_equal(rv$FAT_MAX, r8$FAT_MAX)
})

test_that("step_variable averages a per-step steady-state segment", {
  # Last 120 s of the first step and last 60 s of the others: step 1 VO2 is
  # the mean of records 4-9 of the example data
  e <- .example()
  r <- .mfo(protocol = MFO_protocol("step_variable", step_duration = c(180, 180),
                                    steady_state_duration = c(120, 60)))
  expect_equal(r$MFO_db$VO2[1], mean(e$MFO$VO2[4:9]), tolerance = 1e-12)
  expect_equal(r$MFO_db$VO2[2], .mfo()$MFO_db$VO2[2], tolerance = 1e-12)
})

test_that("relative protocols cut steps as step protocols", {
  r <- .mfo(protocol = MFO_protocol("relative", step_duration = 180), body_mass = 80)
  expect_equal(r$MFO, .mfo()$MFO)
  expect_equal(r$protocol$load_unit, "W/kg")
})

test_that("Step numbers the steps of the test (0 = resting row)", {
  expect_equal(.mfo()$MFO_db$Step, 1:8)
  expect_equal(.mfo(include_basal = TRUE)$MFO_db$Step, 0:8)
  # "safe" (0.95) removes the last two steps of the example data
  r <- .mfo(RER_cutoff = "safe", min_steps = 1)
  expect_equal(r$MFO_db$Step, 1:6)
  # negative_FAT = "exclude" removes step 8 (FAT < 0)
  r <- .mfo(negative_FAT = "exclude")
  expect_equal(r$MFO_db$Step, 1:7)
})

test_that("load schedule is validated and printed", {
  expect_error(MFO_protocol(load_start = "25", load_increment = 25), "single numbers")
  expect_error(MFO_protocol(load_start = c(25, 50), load_increment = 25), "single numbers")
  p <- MFO_protocol("relative", step_duration = 240, load_start = 0.45,
                    load_increment = 0.15)
  expect_output(print(p), "0.45 \\+ 0.15 W/kg per step")
})

test_that("protocol must come from MFO_protocol() and fit step_time", {
  expect_error(.mfo(protocol = list(type = "step")), "MFO_protocol\\(\\)")
  # 10-s steady state with 20-s records is less than one record
  expect_error(.mfo(protocol = MFO_protocol(steady_state_duration = 10)),
               "at least step_time \\(20 s\\)")
})
