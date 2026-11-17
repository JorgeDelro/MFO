# simulate_MFO_sessions() and write_MFO_sessions() (SPEC 12, 13). The key
# test: without noise or day-to-day variation, and with one step exactly at
# Fatmax, MFO() and MFO_kinetics() recover the truth. MV and the normalisation
# of the SIN by the highest step need a step at Fatmax; P3 is not a SIN curve
# and is only close.

.steps <- seq(20, 75, length.out = 8)   # default intensity and n_steps

.fixed <- function(n_sessions = 1, ...) {
  simulate_MFO_sessions(n_participants = 1, n_sessions = n_sessions,
                        population = list(VO2max = c(3000, 0), MFO = c(0.4, 0),
                                          Fatmax = c(.steps[4], 0)),
                        cv_MFO = 0, cv_Fatmax = 0, ...)
}

.run <- function(tabs, author = "Frayn", ...) {
  suppressMessages(MFO(step_time = 20, db_MFO = tabs$db_MFO, db_basal = tabs$db_basal,
                       db_graded = tabs$db_graded, cv_var = "RER", author = author,
                       energy_equivalents = "classic", age = 30, ...))
}

test_that("noiseless tests recover MFO, Fatmax, d, s, t and Fatmin exactly", {
  for (shape in c("basic", "trained")) {
    for (author in c("Frayn", "Peronnet", "Jeukendrup")) {
      sim <- .fixed(shape = shape, author = author)
      tr <- sim$truth
      r <- suppressWarnings(.run(sim$data$P01$S1, author = author, method = "MV"))
      # floating point only
      expect_equal(r$MFO, tr$MFO, tolerance = 1e-12)
      expect_equal(r$FAT_MAX, tr$Fatmax, tolerance = 1e-12)
      expect_equal(r$VO2max, tr$VO2max, tolerance = 1e-12)
      # nlsLM converges to the exact curve; tolerance 1e-6 for the optimiser
      k <- MFO_kinetics(r$MFO_db)
      expect_equal(c(k$d, k$s, k$t, k$Fatmin, k$FatMax_SIN),
                   c(tr$d, tr$s, tr$t, tr$Fatmin, tr$Fatmax), tolerance = 1e-6)
    }
  }
  # truth follows the closed forms: basic shape, Fatmax 43.57 -> Fatmin 93.57
  tr <- .fixed()$truth
  expect_equal(tr$Fatmin, tr$Fatmax + 50)
  expect_equal(tr$t, pi / 2 - pi / 100 * tr$Fatmax)
  # P3 is close but not exact
  r3 <- .run(.fixed()$data$P01$S1)
  expect_equal(r3$FAT_MAX, tr$Fatmax, tolerance = 0.01)
  expect_false(isTRUE(all.equal(r3$FAT_MAX, tr$Fatmax, tolerance = 1e-6)))
  # the graded test meets the maximality criteria
  expect_equal(r3$VO2max_type, "VO2max")
})

test_that("population, session CVs and familiarization behave as stated", {
  sim <- simulate_MFO_sessions(4000, 1, population = list(VO2max = c(3000, 500)),
                               cv_MFO = 0, cv_Fatmax = 0, n_steps = 2, seed = 11)
  p <- sim$participants
  # Amaro-Gahete et al. (2018): 0.34 +/- 0.10 g/min (SE of the mean 0.0016)
  expect_equal(mean(p$MFO), 0.34, tolerance = 0.01 / 0.34)
  expect_equal(sd(p$MFO), 0.10, tolerance = 0.01 / 0.10)
  expect_true(all(p$Fatmax >= 20 & p$Fatmax <= 75))
  expect_true(all(p$MFO > 0))
  # between-session CV of the multiplicative factors (2000 sessions)
  sim <- simulate_MFO_sessions(1, 2000, population = list(VO2max = c(3000, 0),
                               MFO = c(0.4, 0), Fatmax = c(45, 0)), n_steps = 2, seed = 12)
  cv <- function(x) sd(x) / mean(x)
  expect_equal(cv(sim$truth$MFO), 0.21, tolerance = 0.02 / 0.21)
  expect_equal(cv(sim$truth$Fatmax), 0.26, tolerance = 0.02 / 0.26)
  expect_equal(mean(sim$truth$MFO), 0.4, tolerance = 0.01 / 0.4)
  # session 1 shifted by -10 % (MFO) and +5 % (Fatmax)
  sim <- simulate_MFO_sessions(2, 2, population = list(VO2max = c(3000, 0),
                               MFO = c(0.4, 0), Fatmax = c(40, 0)),
                               cv_MFO = 0, cv_Fatmax = 0,
                               familiarization = c(MFO = -0.1, Fatmax = 0.05))
  expect_equal(sim$truth$MFO, rep(c(0.36, 0.4), 2))
  expect_equal(sim$truth$Fatmax, rep(c(42, 40), 2))
})

test_that("analyser noise has the requested CV and correlation", {
  args <- list(n_participants = 40, n_sessions = 1,
               population = list(VO2max = c(3000, 400)), cv_MFO = 0, cv_Fatmax = 0,
               seed = 21)
  clean <- do.call(simulate_MFO_sessions, args)
  noisy <- do.call(simulate_MFO_sessions, c(args, noise_cv = 0.05, noise_cor = 0.8))
  e <- function(sim) do.call(rbind, lapply(sim$data, function(p) p$S1$db_MFO))
  e_vo2 <- e(noisy)$VO2 / e(clean)$VO2 - 1
  e_vco2 <- e(noisy)$VCO2 / e(clean)$VCO2 - 1
  # 2880 records: SE of the SD about 0.0007, of the correlation about 0.007
  expect_equal(sd(e_vo2), 0.05, tolerance = 0.005 / 0.05)
  expect_equal(sd(e_vco2), 0.05, tolerance = 0.005 / 0.05)
  expect_equal(cor(e_vo2, e_vco2), 0.8, tolerance = 0.03 / 0.8)
  expect_equal(noisy$truth, clean$truth)
})

test_that("seed gives reproducible results and restores the random state", {
  a <- simulate_MFO_sessions(3, 2, population = list(VO2max = c(3000, 500)), seed = 7)
  b <- simulate_MFO_sessions(3, 2, population = list(VO2max = c(3000, 500)), seed = 7)
  expect_identical(a, b)
  set.seed(99); x1 <- stats::runif(1)
  set.seed(99); invisible(simulate_MFO_sessions(2, 1, population = list(VO2max = c(3000, 500)),
                                                 seed = 7)); x2 <- stats::runif(1)
  expect_identical(x1, x2)
})

test_that("protocols, loads and per-participant shapes are used", {
  p <- MFO_protocol("step_variable", step_duration = c(300, 180),
                    load_start = 25, load_increment = 25)
  sim <- simulate_MFO_sessions(2, 1, population = list(VO2max = c(3000, 0)),
                               shape = list(d = c(0, 0.18), s = c(1, 1.52)),
                               protocol = p, cv_MFO = 0, cv_Fatmax = 0, seed = 3)
  db <- sim$data$P01$S1$db_MFO
  # 15 + 7 x 9 records of 20 s
  expect_equal(nrow(db), 15 + 7 * 9)
  expect_equal(unique(db$Load), seq(25, 200, by = 25))
  expect_equal(sim$participants$d, c(0, 0.18))
  r <- suppressWarnings(.run(sim$data$P02$S1, protocol = p, method = "MV"))
  expect_equal(r$load_source, "file")
  expect_null(simulate_MFO_sessions(1, 1, population = list(VO2max = c(3000, 0)),
                                    seed = 1)$data$P01$S1$db_MFO$Load)
  expect_output(print(sim), "2 participant\\(s\\) x 1 session\\(s\\), 8 steps of step_variable")
})

test_that("inputs are checked", {
  expect_error(simulate_MFO_sessions(2, 1), "VO2max = c\\(mean, sd\\)")
  pop <- list(VO2max = c(3000, 0))
  expect_error(simulate_MFO_sessions(2, 1, pop, protocol = MFO_protocol("ramp")),
               "step_variable")
  expect_error(simulate_MFO_sessions(2, 1, pop, step_time = 25), "multiples of step_time")
  expect_error(simulate_MFO_sessions(2, 1, pop, shape = list(d = c(0, 0, 0), s = 1)),
               "one per participant")
  expect_error(simulate_MFO_sessions(2, 1, pop, shape = "elite"))
  expect_error(simulate_MFO_sessions(2, 1, pop, intensity = c(80, 20)), "increasing")
  expect_error(simulate_MFO_sessions(0, 1, pop), "whole number")
  expect_error(simulate_MFO_sessions(2, 1, pop, noise_cor = 2), "between -1 and 1")
  expect_error(simulate_MFO_sessions(2, 1, pop, cv_MFO = -1), ">= 0")
  expect_error(simulate_MFO_sessions(2, 1, pop, familiarization = c(a = 1, b = 2)),
               "c\\(MFO")
  expect_error(simulate_MFO_sessions(2, 1, list(VO2max = c(3000, 0), Fatmax = c(90, 0))),
               "outside")
  expect_error(simulate_MFO_sessions(2, 1, list(VO2max = 3000)), "c\\(mean, sd\\)")
})

test_that("write_MFO_sessions() writes folders that MFOs() reads", {
  root <- withr::local_tempdir()
  sim <- .fixed(n_sessions = 2)
  folders <- write_MFO_sessions(sim, root)
  expect_equal(basename(folders), c("P01_S1", "P01_S2"))
  expect_true(file.exists(file.path(root, "P01_S1", "MFO_df.xlsx")))
  index <- utils::read.csv(file.path(root, "sessions.csv"))
  expect_equal(index, data.frame(id = c("P01_S1", "P01_S2"), participant = "P01",
                                 session = c("S1", "S2")))
  res <- suppressWarnings(suppressMessages(
    MFOs(from = "folder", path = root, db_basal_name = "basal_df",
         db_MFO_name = "MFO_df", db_graded_name = "VO2max_df", step_time = 20,
         cv_var = "RER", author = "Frayn", method = "MV",
         energy_equivalents = "classic", save_plot = FALSE, save_result = FALSE)))
  expect_equal(res$ID, c("P01_S1", "P01_S2"))
  expect_equal(res$MFO_g_min, round(sim$truth$MFO, 4))
  expect_equal(res$FAT_MAX_perVO2max, round(sim$truth$Fatmax, 2))
  expect_error(write_MFO_sessions(sim, root), "overwrite = TRUE")
  expect_silent(write_MFO_sessions(sim, root, overwrite = TRUE))
  expect_error(write_MFO_sessions(list(), root), "simulate_MFO_sessions")
})
