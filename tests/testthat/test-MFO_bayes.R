# MFO_bayes() and MFO_prior_check() (SPEC 13.2). The algebra of the
# parametrisation always runs; the Stan fits run only in the separate
# workflow (MFO_STAN_TESTS=true) because compiling takes about a minute.

test_that("d and t are recovered from Fatmax, Fatmin and s (closed forms of SPEC 6.5)", {
  set.seed(1)
  d <- rnorm(5000, 0, 0.4)
  s <- rlnorm(5000, 0, 0.4)
  t <- rnorm(5000, 0.3, 0.4)
  dt <- .sin_dt_from_extrema(.sin_fatmax(d, s, t), .sin_fatmin(d, s, t), s)
  expect_equal(dt$d, d, tolerance = 1e-12)
  expect_equal(dt$t, t, tolerance = 1e-12)
  # the trained shape of Cheneviere et al. (2009, Fig. 5)
  dt <- .sin_dt_from_extrema(.sin_fatmax(0.18, 1.52, 0.21), .sin_fatmin(0.18, 1.52, 0.21), 1.52)
  expect_equal(c(dt$d, dt$t), c(0.18, 0.21), tolerance = 1e-12)
})

test_that("fatmin_prior is checked", {
  expect_error(.check_fatmin_prior(c(90, -1)), "sd > 0")
  expect_error(.check_fatmin_prior(90), "c\\(mean, sd\\)")
  expect_null(.bayes_stanvars("P3", c(90, 14)))
})

test_that("formulas, priors and inputs are built without compiling Stan", {
  skip_if_not_installed("brms")
  for (model in c("SIN", "P3")) {
    for (sigma in c("constant", "proportional")) {
      f <- .bayes_formula(model, sigma)
      expect_s3_class(f, "brmsformula")
      expect_s3_class(.bayes_priors(model, sigma), "brmsprior")
      expect_equal(!is.null(f$pforms$sigma), sigma == "proportional")
    }
  }
  expect_true(all(c("M", "s", "Fmax", "g") %in% names(.bayes_formula("SIN", "constant")$pforms)))
  db <- data.frame(porc_VO2 = seq(20, 75, length.out = 8), FAT = 0.3)
  expect_error(.bayes_data(db[1:4, ]), "at least 5 steps")
  expect_equal(.bayes_data(db)$z, (db$porc_VO2 - 50) / 25)
  expect_error(MFO_bayes(db, conf = 2), "conf must be")
  expect_error(MFO_bayes(db, fatmin_prior = 90), "c\\(mean, sd\\)")
  expect_equal(unlist(.bayes_init("SIN", 2)[[2]]), c(b_M = 0.4, b_s = 1, b_Fmax = 50, b_g = 0))
  expect_equal(.bayes_init("P3", 2), "random")
})

.stan_ok <- function() {
  skip_if_not(identical(Sys.getenv("MFO_STAN_TESTS"), "true"),
              "Stan tests run in the stan workflow (MFO_STAN_TESTS=true)")
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
}

.bayes_case <- function(cv = 0.03) {
  sim <- simulate_MFO_sessions(n_participants = 1, n_sessions = 1,
                               population = list(VO2max = c(3000, 0), MFO = c(0.4, 0),
                                                 Fatmax = c(45, 0)),
                               cv_MFO = 0, cv_Fatmax = 0, shape = "trained",
                               fat_noise_cv = cv, seed = 3)
  tb <- sim$data$P01$S1
  db <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = tb$db_MFO, db_basal = tb$db_basal, db_graded = tb$db_graded,
        cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30,
        method = "MV", negative_FAT = "keep")))$MFO_db
  list(db = db, truth = sim$truth)
}

test_that("MFO_bayes() SIN recovers MFO, Fatmax and Fatmin within its intervals", {
  .stan_ok()
  x <- .bayes_case()
  b <- suppressWarnings(MFO_bayes(x$db, chains = 2, iter = 1000, seed = 1))
  s <- b$summary
  inside <- function(q, v) s[q, "lower"] <= v && v <= s[q, "upper"]
  expect_true(inside("MFO", x$truth$MFO))
  expect_true(inside("Fatmax", x$truth$Fatmax))
  expect_true(inside("Fatmin", x$truth$Fatmin))
  expect_true(all(b$draws$Fatmin > b$draws$Fatmax & b$draws$Fatmin < 100))
  expect_equal(rownames(s), c("MFO", "Fatmax", "Fatmin", "zone_lower", "zone_upper",
                              "d", "s", "t"))
  expect_true(all(s$contraction[s$quantity %in% c("MFO", "Fatmax")] > 0.5))
  expect_output(print(b), "poorly identified among themselves")
  expect_output(print(b), "divergent transition")
  expect_s3_class(plot(b), "ggplot")
  # derived d and t follow the closed forms draw by draw
  expect_equal(.sin_fatmax(b$draws$d, b$draws$s, b$draws$t), b$draws$Fatmax, tolerance = 1e-8)
})

test_that("MFO_bayes() P3 and the proportional sigma run", {
  .stan_ok()
  x <- .bayes_case()
  b <- suppressWarnings(MFO_bayes(x$db, model = "P3", sigma = "proportional",
                                  chains = 2, iter = 1000, seed = 1))
  expect_equal(b$model, "P3")
  expect_true(b$summary["Fatmax", "lower"] < b$summary["Fatmax", "upper"])
  expect_true(is.na(b$diagnostics$max_abs_cor))
})

test_that("MFO_prior_check() draws curves from the priors with Fatmax < Fatmin < 100", {
  .stan_ok()
  x <- .bayes_case()
  pc <- suppressWarnings(MFO_prior_check(x$db, ndraws = 100, seed = 2))
  expect_equal(nrow(pc$draws), 100)
  expect_true(all(pc$draws$Fatmax > 10 & pc$draws$Fatmax < 90))
  expect_true(all(pc$draws$Fatmin > pc$draws$Fatmax & pc$draws$Fatmin < 100))
  expect_equal(rownames(pc$summary), c("MFO", "Fatmax", "Fatmin"))
  expect_s3_class(plot(pc), "ggplot")
  expect_output(print(pc), "prior draws")
})
