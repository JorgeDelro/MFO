# Coverage of the SIN intervals of MFO_kinetics() (SPEC 13, 17). Not run by
# R CMD check; about an hour per fit. From the package source folder:
#   Rscript -e "devtools::load_all(); source('inst/validation/sin_coverage.R')" \
#     --args [absolute|normalised] out.csv
# Resumable: each cell is appended when done and done cells are skipped.
#
# Design (Jorge, 2026-11-18): 8 steps, noise CV 5/10/20 % on FAT, ci =
# "parametric", B = 1000, 200 tests per cell; same simulated participants as
# inst/validation/sin_noise.R (trained shape, MFO 0.4 g/min, Fatmax 45 +/- 8).
# Coverage and mean width of Fatmax, Fatmin and (absolute fit) MFO_SIN.

args <- setdiff(commandArgs(trailingOnly = TRUE), "--args")
fit <- if (length(args) >= 1) args[1] else "absolute"
out_file <- if (length(args) >= 2) args[2] else paste0("sin_coverage_", fit, ".csv")
if (!exists("simulate_MFO_sessions")) library(MFO)
R <- 200
B <- 1000
cvs <- c(0.05, 0.10, 0.20)
done <- if (file.exists(out_file)) utils::read.csv(out_file) else NULL

for (i in seq_along(cvs)) {
  cv <- cvs[i]
  if (!is.null(done) && cv %in% done$fat_noise_cv) {
    message(sprintf("%s CV %.2f: done, skipped", fit, cv))
    next
  }
  message(sprintf("%s CV %.2f", fit, cv))
  set.seed(2100 + i)
  out <- t(replicate(R, {
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
    k <- tryCatch(suppressWarnings(MFO_kinetics(db, fit = fit, ci = "parametric", B = B)),
                  error = function(e) NULL)
    if (is.null(k) || is.null(k$ci)) return(rep(NA_real_, 7))
    ci <- k$ci
    inside <- function(q, truth) ci[q, "lower"] <= truth && truth <= ci[q, "upper"]
    width <- function(q) ci[q, "upper"] - ci[q, "lower"]
    c(cov_fatmax = inside("FatMax_SIN", tr$Fatmax), cov_fatmin = inside("Fatmin", tr$Fatmin),
      cov_mfo = if (fit == "absolute") inside("MFO_SIN", tr$MFO) else NA,
      w_fatmax = width("FatMax_SIN"), w_fatmin = width("Fatmin"),
      w_mfo = if (fit == "absolute") width("MFO_SIN") else NA,
      kept = k$bootstrap$kept / B)
  }))
  ok <- !is.na(out[, 1])
  row <- data.frame(fit = fit, steps = 8, fat_noise_cv = cv, tests = sum(ok),
                    failed = sum(!ok),
                    coverage_fatmax = mean(out[ok, 1]), coverage_fatmin = mean(out[ok, 2]),
                    coverage_mfo = mean(out[ok, 3]),
                    median_width_fatmax = stats::median(out[ok, 4]),
                    median_width_fatmin = stats::median(out[ok, 5]),
                    median_width_mfo = stats::median(out[ok, 6]),
                    replicates_kept = mean(out[ok, 7]))
  utils::write.table(row, out_file, sep = ",", row.names = FALSE,
                     col.names = !file.exists(out_file), append = file.exists(out_file))
}
print(utils::read.csv(out_file), digits = 3)
