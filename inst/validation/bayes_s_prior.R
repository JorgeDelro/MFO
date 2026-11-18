# Sensitivity of MFO_bayes() to the prior of s and to the number of
# iterations (Jorge, 2026-11-18). Same simulated tests as bayes_study.R
# (same seeds), Fatmax + Fatmin parametrisation. From the package source:
#   Rscript -e "devtools::load_all(); source('inst/validation/bayes_s_prior.R')" --args <part> <out.csv>
# part "sprior": s ~ LogNormal(0, 0.5) and (0, 0.6) in the sedentary cell at
# 5 % noise and the trained cell at 10 %; part "iter": iter = 4000 with the
# default s prior in both trained cells; part "sprior4000": s ~ LogNormal(0,
# 0.5) with iter = 4000 in the sedentary 5 % and trained 10 % cells (the
# condition for adopting it, Jorge 2026-11-18). Resumable.
source(file.path("inst", "validation", "bayes_options.R"))
args <- setdiff(commandArgs(trailingOnly = TRUE), "--args")
part <- args[1]
out <- args[2]
K <- pi / 100
d_trained <- (25 * K / (1 - 2^(-1 / 1.52)) - pi) / 2
pops <- list(sedentary = list(fatmax = c(45, 8), shape = "trained"),
             trained = list(fatmax = c(64, 8), shape = list(d = d_trained, s = 1.52)))
mk <- function(cv, i, pop) {
  set.seed(5000 + round(100 * cv) * 1000 + i + if (pop == "trained") 500 else 0)
  sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
    population = list(VO2max = c(3000, 0), MFO = c(0.4, 0), Fatmax = pops[[pop]]$fatmax),
    cv_MFO = 0, cv_Fatmax = 0, shape = pops[[pop]]$shape, fat_noise_cv = cv)
  tb <- sim$data$P01$S1
  db <- suppressWarnings(suppressMessages(MFO(step_time = 20, db_MFO = tb$db_MFO,
    db_basal = tb$db_basal, db_graded = tb$db_graded, cv_var = "RER", author = "Frayn",
    energy_equivalents = "classic", age = 30, method = "MV", negative_FAT = "keep")))$MFO_db
  list(dat = data.frame(x = db$porc_VO2, FAT = db$FAT), truth = sim$truth)
}
prior_s <- function(sd) {
  p <- opt_prior("fatminmax")
  p$prior[p$nlpar == "s"] <- sprintf("lognormal(0, %s)", sd)
  p
}
cells <- if (part == "sprior") {
  expand.grid(s_sd = c(0.5, 0.6), cell = c("sedentary_0.05", "trained_0.10"),
              iter = 2000, stringsAsFactors = FALSE)
} else if (part == "sprior4000") {
  expand.grid(s_sd = 0.5, cell = c("sedentary_0.05", "trained_0.10"), iter = 4000,
              stringsAsFactors = FALSE)
} else {
  expand.grid(s_sd = 0.3, cell = c("trained_0.05", "trained_0.10"), iter = 4000,
              stringsAsFactors = FALSE)
}
done <- if (file.exists(out)) read.csv(out) else NULL
first <- mk(0.05, 1, "sedentary")
models <- list()
for (k in seq_len(nrow(cells))) {
  sd <- cells$s_sd[k]; it <- cells$iter[k]
  pop <- sub("_.*", "", cells$cell[k]); cv <- as.numeric(sub(".*_", "", cells$cell[k]))
  key <- paste(sd, it)
  if (is.null(models[[key]])) {
    models[[key]] <- brm(opt_formula("fatminmax"), data = first$dat, prior = prior_s(sd),
      stanvars = opt_stanvars("fatminmax"), chains = 4, iter = it, seed = 1, refresh = 0,
      silent = 2, init = opt_init("fatminmax", 4), control = list(adapt_delta = 0.99))
  }
  for (i in 1:100) {
    if (!is.null(done) && any(done$s_sd == sd & done$iter == it & done$pop == pop &
                              done$cv == cv & done$test == i)) next
    x <- mk(cv, i, pop); tr <- x$truth
    fit <- suppressWarnings(update(models[[key]], newdata = x$dat, recompile = FALSE, seed = i,
      refresh = 0, silent = 2, init = opt_init("fatminmax", 4), iter = it,
      control = list(adapt_delta = 0.99)))
    p <- opt_post(fit, "fatminmax")
    q <- function(v) quantile(v, c(0.025, 0.975))
    side <- function(v, truth) { r <- q(v); if (truth < r[1]) "truth_below" else if (truth > r[2]) "truth_above" else "inside" }
    np <- nuts_params(fit)
    row <- data.frame(s_sd = sd, iter = it, pop = pop, cv = cv, test = i,
      cov_MFO = side(p$M, tr$MFO) == "inside", side_Fatmax = side(p$Fatmax, tr$Fatmax),
      cov_Fatmin = side(p$Fatmin, tr$Fatmin) == "inside", cov_d = side(p$d, tr$d) == "inside",
      cov_s = side(p$s, tr$s) == "inside", med_err_Fatmax = median(p$Fatmax) - tr$Fatmax,
      divergent = sum(np$Value[np$Parameter == "divergent__"]),
      rhat_max = max(rhat(fit)[grepl("^b_", names(rhat(fit)))], na.rm = TRUE))
    write.table(row, out, sep = ",", row.names = FALSE, col.names = !file.exists(out),
                append = file.exists(out))
    message(sprintf("%s s_sd %s iter %d %s %.2f test %d", part, sd, it, pop, cv, i))
  }
}
