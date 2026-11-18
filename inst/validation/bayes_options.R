# Three prior/parametrisation options for the Bayesian SIN compared in
# vignette("bayes") (Jorge, 2026-11-18): the priors first proposed in SPEC
# 13.2 on d, s, t ("spec"); t replaced by Fatmax ("fatmax"); and Fatmax +
# Fatmin ("fatminmax", the parametrisation of MFO_bayes()). Sourced by
# bayes_study.R; run from the package source folder after devtools::load_all().
library(brms)
clamp <- "M * sin(fmin(fmax(pi()^(1 / s) / (pi() + 2 * dd) * (pi() / 100 * x + dd + tt), 0)^s, pi()))"

opt_formula <- function(opt) {
  if (opt == "spec") {
    bf(FAT ~ M * sin(fmin(fmax(pi()^(1 / s) / (pi() + 2 * d) * (pi() / 100 * x + d + t), 0)^s, pi())),
       M + d + s + t ~ 1, nl = TRUE)
  } else if (opt == "fatmax") {
    bf(as.formula(paste("FAT ~", clamp)),
       nlf(dd ~ d), nlf(tt ~ (pi() + 2 * d) * 2^(-1 / s) - d - pi() / 100 * Fx),
       M + d + s + Fx ~ 1, nl = TRUE)
  } else {
    bf(as.formula(paste("FAT ~", clamp)),
       nlf(Fmin ~ Fx + (100 - Fx) * inv_logit(g)),
       nlf(dd ~ (pi() * (1 - 2^(-1 / s)) + pi() / 100 * (Fx - Fmin)) / (2 * (2^(-1 / s) - 1))),
       nlf(tt ~ pi() + dd - pi() / 100 * Fmin),
       M + s + Fx + g ~ 1, nl = TRUE)
  }
}
opt_prior <- function(opt) {
  base <- c(prior_string("normal(0.45, 0.25)", nlpar = "M", lb = 0),
            prior_string("lognormal(0, 0.3)", nlpar = "s", lb = 0),
            prior_string("exponential(10)", class = "sigma"))
  if (opt == "spec") c(base, prior_string("normal(0, 0.3)", nlpar = "d"),
                       prior_string("normal(0, 0.2)", nlpar = "t"))
  else if (opt == "fatmax") c(base, prior_string("normal(0, 0.3)", nlpar = "d"),
                              prior_string("normal(44.2, 12.4)", nlpar = "Fx", lb = 10, ub = 90))
  else c(base, prior_string("normal(50, 15)", nlpar = "Fx", lb = 10, ub = 90),
         prior_string("normal(0, 10)", nlpar = "g"))
}
# Fatmin ~ Normal(90, 14) truncated to (Fatmax, 100) on Fmin = Fx + (100 - Fx) inv_logit(g):
# Jacobian log(100 - Fx) + log(p) + log(1 - p); the normal(0, 10) on g is cancelled
opt_stanvars <- function(opt) {
  if (opt != "fatminmax") return(NULL)
  stanvar(scode = "
  {
    real p = inv_logit(b_g[1]);
    real Fm = b_Fx[1] + (100 - b_Fx[1]) * p;
    target += normal_lpdf(Fm | 90, 14)
              - log_diff_exp(normal_lcdf(100 | 90, 14), normal_lcdf(b_Fx[1] | 90, 14))
              + log(100 - b_Fx[1]) + log(p) + log1m(p)
              - normal_lpdf(b_g[1] | 0, 10);
  }", block = "model")
}
opt_init <- function(opt, chains) {
  one <- switch(opt,
    spec = list(b_M = as.array(0.4), b_d = as.array(0), b_s = as.array(1), b_t = as.array(0)),
    fatmax = list(b_M = as.array(0.4), b_d = as.array(0), b_s = as.array(1), b_Fx = as.array(50)),
    fatminmax = list(b_M = as.array(0.4), b_s = as.array(1), b_Fx = as.array(50), b_g = as.array(0)))
  lapply(seq_len(chains), function(i) one)
}
# posterior draws of M, d, s, t, Fatmax, Fatmin
opt_post <- function(fit, opt) {
  a <- as.data.frame(as_draws_df(fit)); K <- pi / 100
  if (opt == "spec") { M <- a$b_M_Intercept; d <- a$b_d_Intercept; s <- a$b_s_Intercept; t <- a$b_t_Intercept }
  else if (opt == "fatmax") { M <- a$b_M_Intercept; d <- a$b_d_Intercept; s <- a$b_s_Intercept
    Fx <- a$b_Fx_Intercept; t <- (pi + 2 * d) * 2^(-1 / s) - d - K * Fx }
  else { M <- a$b_M_Intercept; s <- a$b_s_Intercept; Fx <- a$b_Fx_Intercept
    Fm <- Fx + (100 - Fx) * plogis(a$b_g_Intercept); aa <- 2^(-1 / s)
    d <- (pi * (1 - aa) + K * (Fx - Fm)) / (2 * (aa - 1)); t <- pi + d - K * Fm }
  data.frame(M = M, d = d, s = s, t = t, Fatmax = .sin_fatmax(d, s, t), Fatmin = .sin_fatmin(d, s, t))
}
# prior draws (sampled in R from the same priors) for the contraction
rtn <- function(n, m, sd, lo, hi) { u <- runif(n, pnorm(lo, m, sd), pnorm(hi, m, sd)); qnorm(u, m, sd) }
opt_prior_draws <- function(opt, n = 20000) {
  K <- pi / 100; s <- rlnorm(n, 0, 0.3)
  if (opt == "spec") { d <- rnorm(n, 0, 0.3); t <- rnorm(n, 0, 0.2) }
  else if (opt == "fatmax") { d <- rnorm(n, 0, 0.3); Fx <- rtn(n, 44.2, 12.4, 10, 90)
    t <- (pi + 2 * d) * 2^(-1 / s) - d - K * Fx }
  else { Fx <- rtn(n, 50, 15, 10, 90); Fm <- rtn(n, 90, 14, Fx, 100); aa <- 2^(-1 / s)
    d <- (pi * (1 - aa) + K * (Fx - Fm)) / (2 * (aa - 1)); t <- pi + d - K * Fm }
  data.frame(d = d, s = s, t = t)
}
