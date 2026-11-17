# Coverage of the bootstrap confidence intervals of MFO() (SPEC 13.1, 17).
#
# Not run by R CMD check or the tests: it takes about an hour. From the
# package source folder:
#   Rscript inst/validation/bootstrap_coverage.R [bootstrap|parametric] [out.csv]
# with the package installed, or after devtools::load_all().
#
# Design (Jorge, 2026-11-17): method (P3, P2, MRER) x steps (6, 8, 10) x
# noise CV (5, 10, 20 %), R simulated tests per cell, one participant per test
# drawn from the default population of simulate_MFO_sessions() (sedentary
# adults, Amaro-Gahete et al. 2018). The noise is on fat oxidation: the FAT of
# each step deviates from the true curve with SD = CV x MFO (SPEC 17); there
# is no analyser noise on the gases. The truth is the Fatmax and MFO of the
# generating SIN curve, and the model bias of each method (mean estimate minus
# truth) is reported apart: a polynomial fitted to a SIN curve is biased
# whatever the interval.

args <- setdiff(commandArgs(trailingOnly = TRUE), "--args")
ci_type <- if (length(args) >= 1) args[1] else "bootstrap"
out_file <- if (length(args) >= 2) args[2] else
  paste0("bootstrap_coverage_", ci_type, ".csv")

if (!exists("simulate_MFO_sessions")) library(MFO)

R <- 200            # simulated tests per cell
B <- 400            # bootstrap replicates per test
conf <- 0.95
methods <- c("P3", "P2", "MRER")
steps <- c(6, 8, 10)
fat_noise_cv <- c(0.05, 0.10, 0.20)
seed <- 2026

one_cell <- function(method, n_steps, cv, cell_seed) {
  set.seed(cell_seed)
  out <- replicate(R, {
    sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
                                 population = list(VO2max = c(3000, 400)),
                                 cv_MFO = 0, cv_Fatmax = 0, n_steps = n_steps,
                                 fat_noise_cv = cv)
    tabs <- sim$data$P01$S1
    tr <- sim$truth
    r <- tryCatch(suppressWarnings(suppressMessages(
      MFO(step_time = sim$settings$step_time, db_MFO = tabs$db_MFO,
          db_basal = tabs$db_basal, db_graded = tabs$db_graded, cv_var = "RER",
          author = "Frayn", energy_equivalents = "classic", age = 30,
          method = method, ci = ci_type, B = B, conf = conf, min_steps = 1,
          negative_FAT = "keep"))), error = function(e) NULL)
    if (is.null(r) || is.null(r$ci)) return(rep(NA_real_, 7))
    c(cov_fatmax = r$ci["FAT_MAX", "lower"] <= tr$Fatmax && tr$Fatmax <= r$ci["FAT_MAX", "upper"],
      cov_mfo = r$ci["MFO", "lower"] <= tr$MFO && tr$MFO <= r$ci["MFO", "upper"],
      width_fatmax = r$ci["FAT_MAX", "upper"] - r$ci["FAT_MAX", "lower"],
      err_fatmax = r$FAT_MAX - tr$Fatmax,
      err_mfo = r$MFO - tr$MFO,
      rel_err_mfo = (r$MFO - tr$MFO) / tr$MFO,
      kept = r$bootstrap$kept / B)
  })
  ok <- !is.na(out[1, ])
  data.frame(ci = ci_type, method = method, steps = n_steps, fat_noise_cv = cv,
             tests = sum(ok), failed = sum(!ok),
             coverage_fatmax = mean(out[1, ok]), coverage_mfo = mean(out[2, ok]),
             width_fatmax = mean(out[3, ok]),
             bias_fatmax = mean(out[4, ok]), rmse_fatmax = sqrt(mean(out[4, ok]^2)),
             bias_mfo_pct = 100 * mean(out[6, ok]),
             replicates_kept = mean(out[7, ok]))
}

grid <- expand.grid(method = methods, steps = steps, fat_noise_cv = fat_noise_cv,
                    stringsAsFactors = FALSE)
results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  message(sprintf("%s %d/%d: %s, %d steps, FAT CV %.2f", ci_type, i, nrow(grid),
                  grid$method[i], grid$steps[i], grid$fat_noise_cv[i]))
  one_cell(grid$method[i], grid$steps[i], grid$fat_noise_cv[i], seed + i)
}))
print(results, digits = 3)
utils::write.csv(results, out_file, row.names = FALSE)
