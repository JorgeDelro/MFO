# Bias and typical error of the SIN fit with noise, normalised vs absolute
# (SPEC 13.2). Not run by R CMD check. From the package source folder:
#   Rscript -e "devtools::load_all(); source('inst/validation/sin_noise.R')" --args out.csv
#
# Design (Jorge, 2026-11-18): 200 simulated tests per noise CV (5, 10 %), 8
# steps, trained shape (d 0.18, s 1.52), MFO 0.4 g/min, Fatmax 45 +/- 8
# %VO2max, FAT error SD = CV x MFO. MFO_db from MFO(method = "MV",
# negative_FAT = "keep"). For the normalised fit, whose scale is the highest
# observed step, that step is compared with the true MFO. Bias = median
# error; typical error = median absolute error (d has heavy tails, so means
# and RMSE are not used); "extreme" counts tests with |error of d| > 1.

args <- setdiff(commandArgs(trailingOnly = TRUE), "--args")
out_file <- if (length(args) >= 1) args[1] else "sin_noise.csv"
if (!exists("simulate_MFO_sessions")) library(MFO)
R <- 200
rows <- list()
for (cv in c(0.05, 0.10)) {
  set.seed(round(100 * cv) + 900)
  e <- t(replicate(R, {
    sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
                                 population = list(VO2max = c(3000, 0), MFO = c(0.4, 0),
                                                   Fatmax = c(45, 8)),
                                 cv_MFO = 0, cv_Fatmax = 0, shape = "trained",
                                 fat_noise_cv = cv)
    tb <- sim$data$P01$S1
    tr <- sim$truth
    db <- suppressWarnings(suppressMessages(
      MFO(step_time = 20, db_MFO = tb$db_MFO, db_basal = tb$db_basal,
          db_graded = tb$db_graded, cv_var = "RER", author = "Frayn",
          energy_equivalents = "classic", age = 30, method = "MV",
          negative_FAT = "keep")))$MFO_db
    kn <- suppressWarnings(MFO_kinetics(db))
    ka <- suppressWarnings(MFO_kinetics(db, fit = "absolute"))
    c(n_d = kn$d - tr$d, a_d = ka$d - tr$d,
      n_fatmax = kn$FatMax_SIN - tr$Fatmax, a_fatmax = ka$FatMax_SIN - tr$Fatmax,
      n_fatmin = kn$Fatmin - tr$Fatmin, a_fatmin = ka$Fatmin - tr$Fatmin,
      n_mfo = max(db$FAT) - tr$MFO, a_mfo = ka$MFO_SIN - tr$MFO)
  }))
  for (fit in c("normalised", "absolute")) {
    p <- if (fit == "normalised") "n_" else "a_"
    for (q in c("d", "fatmax", "fatmin", "mfo")) {
      v <- e[, paste0(p, q)]
      v <- v[is.finite(v)]
      rows[[length(rows) + 1]] <- data.frame(
        fat_noise_cv = cv, fit = fit, quantity = q, tests = length(v),
        median_error = stats::median(v), median_abs_error = stats::median(abs(v)),
        extreme = if (q == "d") sum(abs(v) > 1) else NA_integer_)
    }
  }
}
res <- do.call(rbind, rows)
print(res, digits = 3)
utils::write.csv(res, out_file, row.names = FALSE)
