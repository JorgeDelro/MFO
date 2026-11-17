# Coverage of the bootstrap confidence intervals of MFO() (SPEC 13.1, 17).
#
# Not run by R CMD check or the tests: it takes minutes to hours. Run it from
# the package source folder with
#   Rscript inst/validation/bootstrap_coverage.R
# after devtools::load_all() or with the package installed.
#
# Design (Jorge, 2026-11-17): method (P3, P2, MRER) x steps (6, 8, 10) x
# noise CV, R simulated tests per cell (one participant per test, drawn from
# the default population of simulate_MFO_sessions()). The truth is the Fatmax
# of the generating SIN curve; the model bias of each method (mean estimate
# minus truth) is reported separately, because a polynomial fitted to a SIN
# curve is biased whatever the interval.
#
# The noise is the analyser noise of simulate_MFO_sessions(): a CV per record
# of VO2 and VCO2 with correlation `noise_cor`. Set `noise_cor` and the CV grid
# before running (no recommended value exists).

if (!exists("simulate_MFO_sessions")) library(MFO)

R <- 200            # simulated tests per cell
B <- 500            # bootstrap replicates per test
conf <- 0.95
methods <- c("P3", "P2", "MRER")
steps <- c(6, 8, 10)
noise_cv <- c(0.05, 0.10, 0.15, 0.20)
noise_cor <- NA     # set before running
seed <- 2026

if (is.na(noise_cor)) stop("Set noise_cor at the top of the script.")

one_cell <- function(method, n_steps, cv, cell_seed) {
  set.seed(cell_seed)
  out <- replicate(R, {
    sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
                                 population = list(VO2max = c(3000, 400)),
                                 cv_MFO = 0, cv_Fatmax = 0, n_steps = n_steps,
                                 noise_cv = cv, noise_cor = noise_cor)
    tabs <- sim$data$P01$S1
    truth <- sim$truth$Fatmax
    r <- tryCatch(suppressWarnings(suppressMessages(
      MFO(step_time = sim$settings$step_time, db_MFO = tabs$db_MFO,
          db_basal = tabs$db_basal, db_graded = tabs$db_graded, cv_var = "RER",
          author = "Frayn", energy_equivalents = "classic", age = 30,
          method = method, ci = "bootstrap", B = B, conf = conf,
          min_steps = 1))), error = function(e) NULL)
    if (is.null(r) || is.null(r$ci)) return(c(NA, NA, NA, NA))
    lo <- r$ci["FAT_MAX", "lower"]
    hi <- r$ci["FAT_MAX", "upper"]
    c(covered = lo <= truth && truth <= hi, width = hi - lo,
      error = r$FAT_MAX - truth, kept = r$bootstrap$kept / B)
  })
  ok <- !is.na(out[1, ])
  data.frame(method = method, steps = n_steps, noise_cv = cv, noise_cor = noise_cor,
             tests = sum(ok), failed = sum(!ok),
             coverage = mean(out[1, ok]), mean_width = mean(out[2, ok]),
             bias = mean(out[3, ok]), rmse = sqrt(mean(out[3, ok]^2)),
             replicates_kept = mean(out[4, ok]))
}

grid <- expand.grid(method = methods, steps = steps, noise_cv = noise_cv,
                    stringsAsFactors = FALSE)
results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  message(sprintf("%d/%d: %s, %d steps, CV %.2f", i, nrow(grid), grid$method[i],
                  grid$steps[i], grid$noise_cv[i]))
  one_cell(grid$method[i], grid$steps[i], grid$noise_cv[i], seed + i)
}))
print(results, digits = 3)
utils::write.csv(results, "bootstrap_coverage_results.csv", row.names = FALSE)
