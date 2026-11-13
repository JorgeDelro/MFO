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

# As .mfo() with any db_MFO and without db_graded unless given
.mfo_db <- function(db_MFO, ...) {
  e <- .example()
  withCallingHandlers(
    suppressMessages(MFO(step_time = 20, db_MFO = db_MFO, db_basal = e$basal,
                         cv_var = "RER", author = "Frayn",
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

# --- ramp and single_day (SPEC 14, session 3) ---
# The example data are a step test; read as a ramp they only check the
# windowing arithmetic. Expected values are computed from the same table.

test_that("ramp protocols are validated and printed", {
  p <- MFO_protocol("ramp", load_start = 20, load_increment = 15)
  expect_equal(p$ramp_window, 30)
  expect_equal(p$ramp_step, 30)
  expect_output(print(p), "30 s, one every 30 s")
  expect_output(print(p), "20 \\+ 15 W per min")
  expect_error(MFO_protocol("ramp", ramp_window = 30, ramp_step = 60), "skipped")
  expect_error(MFO_protocol("ramp", ramp_window = c(30, 60)), "single numbers")
  expect_null(MFO_protocol()$ramp_window)
})

test_that("ramp windows are back to back by default and rounded up to records", {
  # 30-s windows with 20-s records: 2 records (40 s), 72 records -> 36 windows
  expect_message(b <- .step_bounds(MFO_protocol("ramp"), 72, 20), "using 2 record")
  expect_equal(b$end, seq(2, 72, by = 2))
  expect_equal(unique(b$steady_records), 2)
  # 60-s windows every 20 s: 70 overlapping windows ending at records 3..72
  b <- .step_bounds(MFO_protocol("ramp", ramp_window = 60, ramp_step = 20), 72, 20)
  expect_equal(b$end, 3:72)
  expect_equal(.independent_points(70, MFO_protocol("ramp", ramp_window = 60,
                                                    ramp_step = 20), 20), 24)
  expect_equal(.independent_points(36, MFO_protocol("ramp", ramp_window = 40), 20), 36)
  expect_equal(nrow(.step_bounds(MFO_protocol("ramp", ramp_window = 60), 2, 20)), 0)
})

test_that("a ramp averages each window and gives loads per window", {
  e <- .example()
  r <- .mfo(protocol = MFO_protocol("ramp", ramp_window = 60), age = 30)
  expect_equal(nrow(r$MFO_db), 24)
  expect_equal(r$MFO_db$VO2[5], mean(e$MFO$VO2[13:15]), tolerance = 1e-12)
  expect_equal(r$MFO_db$Load[5], mean(e$MFO$Load[13:15]), tolerance = 1e-12)
  # schedule: 20 W + 15 W/min at the window midpoint, (k - 1) * 60 + 30 s
  no_load <- e$MFO[, setdiff(names(e$MFO), "Load")]
  p <- MFO_protocol("ramp", ramp_window = 60, load_start = 20, load_increment = 15)
  r2 <- suppressMessages(.mfo_db(no_load, db_graded = e$graded, protocol = p, age = 30))
  expect_equal(r2$MFO_db$Load, 20 + 15 * ((seq_len(24) - 1) * 60 + 30) / 60)
  expect_equal(r2$load_source, "protocol")
})

test_that("a ramp refuses MV and counts non-overlapping windows for min_steps", {
  expect_error(.mfo(protocol = MFO_protocol("ramp"), method = "MV"), "use \"P3\"")
  # 60-s windows every 20 s: 70 points but 24 independent windows
  expect_warning(.mfo(protocol = MFO_protocol("ramp", ramp_window = 60, ramp_step = 20),
                      min_steps = 25, age = 30),
                 "24 non-overlapping ramp window")
  expect_no_warning(.mfo(protocol = MFO_protocol("ramp", ramp_window = 60, ramp_step = 20),
                         min_steps = 24, age = 30))
})

test_that("single_day cuts the recording into MFO test and graded test", {
  e <- .example()
  cols <- c("HR", "Load", "VO2", "VCO2", "RER")
  one <- rbind(e$MFO[, cols], e$graded[, cols])
  p <- MFO_protocol("single_day", cut_time = 72 * 20)
  r1 <- .mfo_db(one, protocol = p, age = 30)
  r0 <- .mfo(age = 30)
  r1$MFO_plot <- r0$MFO_plot <- NULL
  r1$protocol <- r0$protocol <- NULL
  expect_equal(r1, r0)
  expect_error(MFO_protocol("single_day"), "needs cut_time")
  expect_error(MFO_protocol(cut_time = 100), "only used")
  expect_error(.mfo_db(one, protocol = MFO_protocol("single_day", cut_time = 1e5)),
               "no graded test")
})

test_that("single_day and ramp edge cases", {
  expect_error(MFO_protocol("single_day", cut_time = c(600, 1200)), "single number")
  expect_output(print(MFO_protocol("single_day", cut_time = 1440)),
                "first 1440 s; graded test after")
  expect_equal(.independent_points(0, MFO_protocol("ramp"), 20), 0)
  expect_equal(.independent_points(7, MFO_protocol(), 20), 7)
})
