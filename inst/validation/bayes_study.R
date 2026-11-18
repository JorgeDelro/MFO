# Reduced study of vignette("bayes") (Jorge, 2026-11-18): 2 populations x
# noise CV 5/10 % x 100 tests x 3 options. One row per test and option is
# appended to the output, so the script resumes where it stopped. From the
# package source folder:
#   Rscript -e "devtools::load_all(); source('inst/validation/bayes_study.R')"
source(file.path("inst", "validation", "bayes_options.R"))
out <- "bayes_study.csv"
# populations: sedentary (as before) and trained, Fatmax 64 +/- 8 (Achten 2002)
# with Fatmin - Fatmax = 25 (Fatmin about 89): s = 1.52, d solved from the width
K <- pi / 100
d_trained <- (25 * K / (1 - 2^(-1 / 1.52)) - pi) / 2
pops <- list(sedentary = list(fatmax = c(45, 8), shape = "trained"),
             trained = list(fatmax = c(64, 8), shape = list(d = d_trained, s = 1.52)))
opts <- c("spec", "fatmax", "fatminmax")
done <- if (file.exists(out)) read.csv(out) else NULL
# compile each model once on a first dataset, then update()
mk <- function(cv, i, pop = "sedentary") {
  set.seed(5000 + round(100 * cv) * 1000 + i + if (pop == "trained") 500 else 0)
  sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
    population = list(VO2max = c(3000, 0), MFO = c(0.4, 0), Fatmax = pops[[pop]]$fatmax),
    cv_MFO = 0, cv_Fatmax = 0, shape = pops[[pop]]$shape, fat_noise_cv = cv)
  tb <- sim$data$P01$S1
  db <- suppressWarnings(suppressMessages(MFO(step_time = 20, db_MFO = tb$db_MFO, db_basal = tb$db_basal,
    db_graded = tb$db_graded, cv_var = "RER", author = "Frayn", energy_equivalents = "classic",
    age = 30, method = "MV", negative_FAT = "keep")))$MFO_db
  list(dat = data.frame(x = db$porc_VO2, FAT = db$FAT), truth = sim$truth)
}
first <- mk(0.05, 1)
models <- lapply(setNames(opts, opts), function(opt)
  brm(opt_formula(opt), data = first$dat, prior = opt_prior(opt), stanvars = opt_stanvars(opt),
      chains = 4, iter = 2000, seed = 1, refresh = 0, silent = 2, init = opt_init(opt, 4),
      control = list(adapt_delta = 0.99)))
set.seed(1); priors <- lapply(setNames(opts, opts), opt_prior_draws)
for (pop in names(pops)) for (cv in c(0.05, 0.10)) for (i in 1:100) {
  if (!is.null(done) && any(done$pop == pop & done$cv == cv & done$test == i)) next
  x <- mk(cv, i, pop); tr <- x$truth
  rows <- lapply(opts, function(opt) {
    fit <- tryCatch(suppressWarnings(update(models[[opt]], newdata = x$dat, recompile = FALSE,
                    seed = i, refresh = 0, silent = 2, init = opt_init(opt, 4),
                    control = list(adapt_delta = 0.99))), error = function(e) NULL)
    if (is.null(fit)) return(data.frame(pop = pop, cv = cv, test = i, option = opt, ok = FALSE))
    p <- opt_post(fit, opt)
    q <- function(v) quantile(v, c(0.025, 0.975), na.rm = TRUE)
    inside <- function(v, truth) { r <- q(v); r[1] <= truth && truth <= r[2] }
    np <- nuts_params(fit)
    cr <- cor(p[, c("d", "s", "t")]); diag(cr) <- 0
    side <- function(v, truth) { r <- q(v); if (truth < r[1]) "truth_below" else if (truth > r[2]) "truth_above" else "inside" }
    data.frame(pop = pop, cv = cv, test = i, option = opt, ok = TRUE,
               truth_Fatmax = tr$Fatmax, truth_Fatmin = tr$Fatmin,
               med_err_Fatmax = median(p$Fatmax) - tr$Fatmax,
               med_err_Fatmin = median(p$Fatmin) - tr$Fatmin,
               side_Fatmax = side(p$Fatmax, tr$Fatmax), side_Fatmin = side(p$Fatmin, tr$Fatmin),
               cov_MFO = inside(p$M, tr$MFO), cov_Fatmax = inside(p$Fatmax, tr$Fatmax),
               cov_Fatmin = inside(p$Fatmin, tr$Fatmin), cov_d = inside(p$d, tr$d),
               cov_s = inside(p$s, tr$s),
               contraction_d = 1 - var(p$d) / var(priors[[opt]]$d),
               max_abs_cor = max(abs(cr)),
               divergent = sum(np$Value[np$Parameter == "divergent__"]),
               rhat_max = max(rhat(fit)[grepl("^b_", names(rhat(fit)))], na.rm = TRUE))
  })
  r <- do.call(rbind, lapply(rows, function(z) { for (k in setdiff(names(rows[[which.max(sapply(rows, ncol))]]), names(z))) z[[k]] <- NA; z }))
  write.table(r, out, sep = ",", row.names = FALSE, col.names = !file.exists(out), append = file.exists(out))
  message(sprintf("%s cv %.2f test %d", pop, cv, i))
}
