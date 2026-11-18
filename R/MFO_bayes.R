#' Bayesian fit of the fat oxidation curve
#'
#' Fits the SIN model (Cheneviere et al. 2009) in g/min, or a third-degree
#' polynomial, as a Bayesian model with \pkg{brms} (Burkner 2017,
#' \doi{10.18637/jss.v080.i01}) on Stan (Carpenter et al. 2017,
#' \doi{10.18637/jss.v076.i01}), and returns posterior medians and credible
#' intervals for MFO, Fatmax, the Fatmax zone, Fatmin and the SIN parameters
#' (SPEC 13.2). It does not replace [MFO()]: it is a separate analysis of the
#' steps that [MFO()] returns. \pkg{brms} and \pkg{rstan} are in Suggests.
#'
#' **SIN model.** FAT_i ~ Normal(M sin(min(max(u_i, 0)^s, pi)), sigma), with
#' u = pi^(1/s) / (pi + 2d) (K x + d + t), K = pi / 100 and x in \% VO2max.
#' MFO (M) is a parameter, so there is no normalisation by the highest step.
#' Outside the two zeros of the curve u^s is set to 0 or pi, so fat
#' oxidation is 0 there (as in [simulate_MFO_sessions()]); this also keeps
#' non-integer powers of negative numbers out of Stan (SPEC 13.2).
#'
#' The model is parametrised by MFO, Fatmax, Fatmin and s, the quantities the
#' data inform; d and t follow from the closed forms of Fatmax and Fatmin
#' (SPEC 6.5) in each draw, with a = 2^(-1/s):
#' t = pi + d - K Fatmin and d = \[pi (1 - a) + K (Fatmax - Fatmin)\] / \[2 (a - 1)\].
#' The usual parametrisation (d, s, t) has a ridge in the posterior (s and t
#' correlated above 0.9) whose position is set by the priors: with the
#' priors first proposed in SPEC 13.2 (d ~ N(0, 0.3), s ~ LogNormal(0, 0.3),
#' t ~ N(0, 0.2)) the 95 \% intervals of s covered the true value in 7-10 \%
#' of simulated tests where t is far from 0 (see `vignette("bayes")`). Fatmin is constrained to lie
#' between Fatmax and 100 \% VO2max. The Fatmax zone (FAT >= 90 \% of M)
#' also comes from closed forms of each draw.
#'
#' **P3 model.** FAT ~ b0 + b1 z + b2 z^2 + b3 z^3 with z = (x - 50) / 25;
#' Fatmax, MFO and the zone are found on a grid of the measured intensities
#' for each draw.
#'
#' **Priors.** The defaults are the authors' weakly informative choices, not
#' estimates from the literature. SIN:
#' \itemize{
#' \item M ~ Normal(0.45, 0.25) truncated at 0, centred above the normative
#'   mean of sedentary adults (0.34 +/- 0.10 g/min; Amaro-Gahete,
#'   Sanchez-Delgado & Ruiz 2018, Front Physiol 9: 1460, Table 1,
#'   \doi{10.3389/fphys.2018.01460}) with an inflated SD to cover trained
#'   people;
#' \item Fatmax ~ Normal(50, 15) truncated to (10, 90) \% VO2max, centred
#'   between the normative value of sedentary adults (44.2 +/- 12.4; same
#'   table) and that of trained men (64 \%; Achten, Gleeson & Jeukendrup 2002,
#'   \doi{10.1097/00005768-200201000-00015}) with an inflated SD;
#' \item Fatmin ~ Normal(90, 14) truncated to (Fatmax, 100) \% VO2max
#'   (90.1 +/- 14.1 in Cheneviere et al. 2009), set with `fatmin_prior`;
#' \item s ~ LogNormal(0, 0.3).
#' }
#' P3: b0 ~ Normal(0.3, 0.3), b1-b3 ~ Normal(0, 0.3). sigma ~
#' Exponential(10) (constant) or, with `sigma = "proportional"`,
#' sigma_i = sigma_100 x_i / 100 with log(sigma_100) ~ Normal(log(0.1), 1).
#' Replace them with `priors` (a `brmsprior` object, e.g. from
#' [brms::prior()]); check them with [MFO_prior_check()].
#'
#' **What to interpret.** `contraction` = 1 - Var(posterior) / Var(prior)
#' tells how much the data narrowed each quantity; below 0.5 it is labelled
#' `prior-dominated` in `print()`. The shape parameters d, s and t are poorly
#' identified among themselves (`print()` gives their largest posterior
#' correlation): interpret MFO, Fatmax and Fatmin, not d, s and t (see
#' [MFO_kinetics()], *What to interpret*).
#'
#' @param MFO_data the `MFO_db` of an [MFO()] result (columns `porc_VO2` and
#'   `FAT`); every row is used, as in [MFO_kinetics()].
#' @param model `"SIN"` (default) or `"P3"`.
#' @param priors optional `brmsprior` object replacing all default priors.
#' @param sigma `"constant"` (default) or `"proportional"` to intensity.
#' @param fatmin_prior mean and SD of the normal prior of Fatmin, truncated
#'   to (Fatmax, 100) (default `c(90, 14)`, SIN only).
#' @param chains,iter number of chains and iterations per chain (defaults 4
#'   and 2000, half of them warm-up).
#' @param conf level of the credible intervals (default 0.95).
#' @param seed optional integer passed to Stan.
#' @param backend passed to [brms::brm()] (default `"rstan"`).
#' @param adapt_delta target acceptance rate of the sampler (default 0.99;
#'   the SIN likelihood has kinks at the zeros of the curve). Divergent
#'   transitions are always reported.
#' @param ... further arguments to [brms::brm()].
#'
#' @return An object of class `MFO_bayes`: a list with `summary` (one row per
#'   quantity: `median`, `lower`, `upper`, `contraction`),
#'   `draws` (posterior draws of the parameters and derived quantities),
#'   `diagnostics` (maximum R-hat, minimum bulk and tail ESS, divergent
#'   transitions, largest posterior correlation among d, s and t), `model`,
#'   `sigma`, `conf` and `fit` (the `brmsfit`, e.g. for
#'   [brms::loo()]). `plot()` draws posterior curves.
#'
#' @seealso [MFO_prior_check()], [MFO_kinetics()], [MFO()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("brms", quietly = TRUE)) {
#'   data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
#'   r <- MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df,
#'            db_graded = VO2max_df, cv_var = "RER", author = "Frayn",
#'            energy_equivalents = "classic", negative_FAT = "keep")
#'   b <- MFO_bayes(r$MFO_db, seed = 1)
#'   b
#'   plot(b)
#' }
#' }
MFO_bayes <- function(MFO_data, model = c("SIN", "P3"), priors = NULL,
                      sigma = c("constant", "proportional"), fatmin_prior = c(90, 14),
                      chains = 4, iter = 2000, conf = 0.95, seed = NULL,
                      backend = "rstan", adapt_delta = 0.99, ...) {
  model <- match.arg(model)
  sigma <- match.arg(sigma)
  .require_brms()
  if (!is.numeric(conf) || length(conf) != 1 || is.na(conf) || conf <= 0 || conf >= 1) {
    stop("conf must be a single number between 0 and 1.", call. = FALSE)
  }
  .check_fatmin_prior(fatmin_prior)
  dat <- .bayes_data(MFO_data)
  fit <- brms::brm(.bayes_formula(model, sigma), data = dat,
                   prior = if (is.null(priors)) .bayes_priors(model, sigma) else priors,
                   stanvars = .bayes_stanvars(model, fatmin_prior),
                   sample_prior = "yes", chains = chains, iter = iter,
                   seed = if (is.null(seed)) NA else seed, backend = backend,
                   init = .bayes_init(model, chains), refresh = 0, silent = 2,
                   control = list(adapt_delta = adapt_delta), ...)
  .bayes_result(fit, dat, model, sigma, conf, fatmin_prior)
}


#' Prior predictive check of MFO_bayes()
#'
#' Samples the priors of [MFO_bayes()] only (`sample_prior = "only"` in
#' \pkg{brms}) and shows the fat oxidation curves they imply, with the
#' observed steps for reference, and the prior distribution of MFO, Fatmax
#' and Fatmin. Use it to check that the priors allow every plausible curve
#' and few absurd ones before fitting, and, against an [MFO_bayes()] result,
#' to see which parameters the data inform (compare the prior and posterior
#' of d).
#'
#' @inheritParams MFO_bayes
#' @param ndraws number of prior curves drawn (default 200).
#'
#' @return An object of class `MFO_prior_check`: a list with `draws` (prior
#'   draws of the parameters and derived quantities), `summary` (quantiles 5,
#'   50 and 95 \%), `curves` (ndraws curves on 0-100 \% VO2max), `data` and
#'   `model`. `plot()` draws the curves.
#'
#' @seealso [MFO_bayes()]
#'
#' @export
MFO_prior_check <- function(MFO_data, model = c("SIN", "P3"), priors = NULL,
                            sigma = c("constant", "proportional"),
                            fatmin_prior = c(90, 14), ndraws = 200,
                            seed = NULL, backend = "rstan", ...) {
  model <- match.arg(model)
  sigma <- match.arg(sigma)
  .require_brms()
  .check_fatmin_prior(fatmin_prior)
  dat <- .bayes_data(MFO_data)
  fit <- brms::brm(.bayes_formula(model, sigma), data = dat,
                   prior = if (is.null(priors)) .bayes_priors(model, sigma) else priors,
                   stanvars = .bayes_stanvars(model, fatmin_prior),
                   sample_prior = "only", chains = 1, iter = max(2 * ndraws, 1000),
                   seed = if (is.null(seed)) NA else seed, backend = backend,
                   init = .bayes_init(model, 1), refresh = 0, silent = 2, ...)
  draws <- .bayes_draws(fit, model, dat, prefix = "b_")
  idx <- utils::head(seq_len(nrow(draws)), ndraws)
  draws <- draws[idx, , drop = FALSE]
  x <- seq(0, 100, length.out = 101)
  curves <- do.call(rbind, lapply(seq_len(nrow(draws)), function(i) {
    data.frame(draw = i, x = x, FAT = .bayes_curve(draws[i, ], model, x))
  }))
  qs <- c("MFO", "Fatmax", if (model == "SIN") "Fatmin")
  summ <- t(vapply(qs, function(q) stats::quantile(draws[[q]], c(0.05, 0.5, 0.95),
                                                   na.rm = TRUE, names = FALSE),
                   numeric(3)))
  colnames(summ) <- c("q05", "median", "q95")
  structure(list(draws = draws, summary = as.data.frame(summ), curves = curves,
                 data = dat, model = model, fit = fit),
            class = "MFO_prior_check")
}


#' @export
print.MFO_bayes <- function(x, digits = 3, ...) {
  d <- x$diagnostics
  cat(sprintf("<MFO_bayes> %s model, sigma %s, %g %% credible intervals\n", x$model,
              x$sigma, 100 * x$conf))
  s <- x$summary
  s$note <- ifelse(!is.na(s$contraction) & s$contraction < 0.5, "prior-dominated", "")
  num <- vapply(s, is.numeric, logical(1))
  s[num] <- lapply(s[num], signif, digits = digits)
  print(s, row.names = FALSE)
  cat(sprintf("  max R-hat %.3f, min bulk ESS %.0f, min tail ESS %.0f, %d divergent transition(s)\n",
              d$rhat_max, d$ess_bulk_min, d$ess_tail_min, d$divergent))
  if (x$model == "SIN") {
    cat(sprintf("  largest posterior |r| among d, s and t: %.2f\n", d$max_abs_cor))
    cat("  The shape parameters (d, s, t) are poorly identified among themselves;",
        "interpret MFO, Fatmax and Fatmin.\n")
  }
  invisible(x)
}


#' @export
print.MFO_prior_check <- function(x, digits = 3, ...) {
  cat(sprintf("<MFO_prior_check> %s model, %d prior draws\n", x$model, nrow(x$draws)))
  print(signif(x$summary, digits))
  invisible(x)
}


#' @export
plot.MFO_bayes <- function(x, ndraws = 100, ...) {
  dr <- x$draws[utils::head(sample.int(nrow(x$draws)), ndraws), , drop = FALSE]
  rng <- if (x$model == "SIN") c(0, 100) else range(x$data$x)
  grid <- seq(rng[1], rng[2], length.out = 101)
  curves <- do.call(rbind, lapply(seq_len(nrow(dr)), function(i) {
    data.frame(draw = i, x = grid, FAT = .bayes_curve(dr[i, ], x$model, grid))
  }))
  med <- data.frame(x = grid, FAT = apply(
    vapply(seq_len(nrow(x$draws)), function(i) .bayes_curve(x$draws[i, ], x$model, grid),
           numeric(length(grid))), 1, stats::median))
  s <- x$summary
  ci <- function(q, fmt) sprintf(fmt, s[q, "median"], s[q, "lower"], s[q, "upper"])
  subtitle <- paste0(ci("MFO", "MFO = %.3f (%.3f-%.3f) g/min"), " \u00b7 ",
                     ci("Fatmax", "FATmax = %.1f (%.1f-%.1f) %% VO2max"),
                     if (x$model == "SIN") paste0(" \u00b7 ", ci("Fatmin", "Fatmin = %.1f (%.1f-%.1f)")),
                     sprintf(" \u00b7 %s, %g %% CrI", x$model, 100 * x$conf))
  ggplot() +
    geom_line(data = curves, aes(x = x, y = FAT, group = draw), colour = "grey50",
              alpha = 0.15) +
    geom_line(data = med, aes(x = x, y = FAT), colour = "darkorange", linewidth = 1) +
    geom_point(data = x$data, aes(x = x, y = FAT), size = 2.5) +
    labs(x = expression("Exercise intensity (% " * VO[2*max] * ")"),
         y = expression("Fat oxidation (g" %.% "min"^{-1} * ")"),
         subtitle = subtitle,
         caption = "Grey: posterior draws. Orange: posterior median curve.") +
    theme_bw() + theme(plot.caption = element_text(hjust = 0, colour = "grey30"))
}


#' @export
plot.MFO_prior_check <- function(x, ...) {
  ggplot() +
    geom_line(data = x$curves, aes(x = x, y = FAT, group = draw), colour = "grey50",
              alpha = 0.2) +
    geom_point(data = x$data, aes(x = x, y = FAT), size = 2.5) +
    labs(x = expression("Exercise intensity (% " * VO[2*max] * ")"),
         y = expression("Fat oxidation (g" %.% "min"^{-1} * ")"),
         subtitle = sprintf("Prior predictive check, %s model: %d curves from the priors",
                            x$model, nrow(x$draws)),
         caption = "Grey: curves drawn from the priors only. Points: observed steps.") +
    theme_bw() + theme(plot.caption = element_text(hjust = 0, colour = "grey30"))
}


# --- Internal helpers -------------------------------------------------------

.require_brms <- function() {
  for (p in c("brms", "rstan")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("MFO_bayes() needs the packages brms and rstan: install.packages(c(\"brms\", ",
           "\"rstan\")).", call. = FALSE)
    }
  }
}

.bayes_data <- function(MFO_data) {
  dat <- data.frame(x = MFO_data$porc_VO2, FAT = MFO_data$FAT)
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < 5) stop("MFO_bayes() needs at least 5 steps.", call. = FALSE)
  dat$z <- (dat$x - 50) / 25
  dat$z2 <- dat$z^2
  dat$z3 <- dat$z^3
  dat
}

.bayes_formula <- function(model, sigma) {
  sig <- if (sigma == "proportional") stats::as.formula("sigma ~ 1 + offset(log(x / 100))")
  if (model == "SIN") {
    # Parametrised by M, s, Fmax and g: Fmin = Fmax + (100 - Fmax) inv_logit(g)
    # lies in (Fmax, 100); d and t from the closed forms of Fatmax and Fatmin
    # The clamps max(u, 0) and min(v, pi) are written with abs(), and pi as a
    # number, so that the formula is valid both in Stan and in R (brms
    # evaluates it in R for loo() and predictions)
    PI <- "3.141592653589793"
    f <- stats::as.formula(gsub("PI", PI, paste(
      "FAT ~ M * sin((up^s + PI - abs(up^s - PI)) / 2)"), fixed = TRUE))
    parts <- list(f,
                  brms::nlf(stats::as.formula(gsub("PI", PI,
                    "u ~ PI^(1 / s) / (PI + 2 * dd) * (PI / 100 * x + dd + tt)", fixed = TRUE))),
                  brms::nlf(up ~ (u + abs(u)) / 2),
                  brms::nlf(Fmin ~ Fmax + (100 - Fmax) * inv_logit(g)),
                  brms::nlf(stats::as.formula(gsub("PI", PI,
                    "dd ~ (PI * (1 - 2^(-1 / s)) + PI / 100 * (Fmax - Fmin)) / (2 * (2^(-1 / s) - 1))",
                    fixed = TRUE))),
                  brms::nlf(stats::as.formula(gsub("PI", PI, "tt ~ PI + dd - PI / 100 * Fmin",
                                                   fixed = TRUE))),
                  M + s + Fmax + g ~ 1)
    if (!is.null(sig)) parts <- c(parts, list(sig))
    do.call(brms::bf, c(parts, list(nl = TRUE)))
  } else {
    if (is.null(sig)) brms::bf(FAT ~ 1 + z + z2 + z3) else brms::bf(FAT ~ 1 + z + z2 + z3, sig)
  }
}

# Authors' weakly informative choices (see ?MFO_bayes)
.bayes_priors <- function(model, sigma) {
  p <- if (model == "SIN") {
    # g carries the prior of Fatmin through .bayes_stanvars(); its normal(0, 10)
    # is cancelled there
    c(brms::prior_string("normal(0.45, 0.25)", nlpar = "M", lb = 0),
      brms::prior_string("lognormal(0, 0.3)", nlpar = "s", lb = 0),
      brms::prior_string("normal(50, 15)", nlpar = "Fmax", lb = 10, ub = 90),
      brms::prior_string("normal(0, 10)", nlpar = "g"))
  } else {
    c(brms::prior_string("normal(0.3, 0.3)", class = "Intercept"),
      brms::prior_string("normal(0, 0.3)", class = "b"))
  }
  if (sigma == "proportional") {
    c(p, brms::prior_string(sprintf("normal(%.4f, 1)", log(0.1)), class = "Intercept",
                            dpar = "sigma"))
  } else {
    c(p, brms::prior_string("exponential(10)", class = "sigma"))
  }
}

.check_fatmin_prior <- function(fp) {
  if (!is.numeric(fp) || length(fp) != 2 || anyNA(fp) || fp[2] <= 0) {
    stop("fatmin_prior must be c(mean, sd) with sd > 0.", call. = FALSE)
  }
}

# Prior of Fatmin, Normal(m, sd) truncated to (Fmax, 100), on
# Fmin = Fmax + (100 - Fmax) inv_logit(g): log Jacobian log(100 - Fmax) +
# log(p) + log(1 - p); the normal(0, 10) prior of g is removed
.bayes_stanvars <- function(model, fp) {
  if (model != "SIN") return(NULL)
  brms::stanvar(scode = sprintf("
  {
    real p_fmin = inv_logit(b_g[1]);
    real fmin_v = b_Fmax[1] + (100 - b_Fmax[1]) * p_fmin;
    target += normal_lpdf(fmin_v | %1$.6f, %2$.6f)
              - log_diff_exp(normal_lcdf(100 | %1$.6f, %2$.6f),
                             normal_lcdf(b_Fmax[1] | %1$.6f, %2$.6f))
              + log(100 - b_Fmax[1]) + log(p_fmin) + log1m(p_fmin)
              - normal_lpdf(b_g[1] | 0, 10);
  }", fp[1], fp[2]), block = "model")
}

.bayes_init <- function(model, chains) {
  one <- if (model == "SIN") {
    list(b_M = as.array(0.4), b_s = as.array(1), b_Fmax = as.array(50), b_g = as.array(0))
  } else {
    "random"
  }
  if (identical(one, "random")) return("random")
  lapply(seq_len(chains), function(i) one)
}

# d and t of the SIN from Fatmax, Fatmin and s, inverting the closed forms
# of SPEC 6.5 (K Fatmin = pi + d - t; K Fatmax = (pi + 2d) a - d - t,
# a = 2^(-1/s)): t = pi + d - K Fatmin, d = [pi (1 - a) + K (Fatmax -
# Fatmin)] / [2 (a - 1)]
.sin_dt_from_extrema <- function(fatmax, fatmin, s) {
  K <- pi / 100
  a <- 2^(-1 / s)
  d <- (pi * (1 - a) + K * (fatmax - fatmin)) / (2 * (a - 1))
  list(d = d, t = pi + d - K * fatmin)
}

# Fat oxidation curve of one draw at intensities x
.bayes_curve <- function(p, model, x) {
  if (model == "SIN") {
    u <- pi^(1 / p$s) / (pi + 2 * p$d) * (pi / 100 * x + p$d + p$t)
    p$M * sin(pmin(pmax(u, 0)^p$s, pi))
  } else {
    z <- (x - 50) / 25
    p$b0 + p$b1 * z + p$b2 * z^2 + p$b3 * z^3
  }
}

# Parameter draws (posterior, or prior with prefix "prior_b_") and derived
# quantities per draw
.bayes_draws <- function(fit, model, dat, prefix = "b_", fatmin_prior = c(90, 14)) {
  all <- as.data.frame(brms::as_draws_df(fit))
  K <- pi / 100
  if (model == "SIN") {
    if (prefix == "b_") {
      p <- data.frame(M = all$b_M_Intercept, s = all$b_s_Intercept,
                      Fatmax = all$b_Fmax_Intercept)
      p$Fatmin <- p$Fatmax + (100 - p$Fatmax) * stats::plogis(all$b_g_Intercept)
    } else {
      # prior draws of M, s and Fatmax from brms; Fatmin from its truncated
      # normal given Fatmax (its prior is not a brms prior, see .bayes_stanvars)
      p <- data.frame(M = all$prior_b_M, s = all$prior_b_s, Fatmax = all$prior_b_Fmax)
      lo <- stats::pnorm(p$Fatmax, fatmin_prior[1], fatmin_prior[2])
      hi <- stats::pnorm(100, fatmin_prior[1], fatmin_prior[2])
      p$Fatmin <- stats::qnorm(lo + stats::runif(nrow(p)) * (hi - lo), fatmin_prior[1],
                               fatmin_prior[2])
    }
    dt <- .sin_dt_from_extrema(p$Fatmax, p$Fatmin, p$s)
    p$d <- dt$d
    p$t <- dt$t
    p$MFO <- p$M
    lev <- asin(0.9)
    p$zone_lower <- ((pi + 2 * p$d) * (lev / pi)^(1 / p$s) - p$d - p$t) / K
    p$zone_upper <- ((pi + 2 * p$d) * ((pi - lev) / pi)^(1 / p$s) - p$d - p$t) / K
    return(p)
  }
  nm <- c(b0 = "Intercept", b1 = "z", b2 = "z2", b3 = "z3")
  cols <- if (prefix == "b_") paste0(prefix, nm) else c("prior_Intercept", rep("prior_b", 3))
  p <- as.data.frame(lapply(stats::setNames(cols, names(nm)), function(cl) all[[cl]]))
  grid <- seq(min(dat$x), max(dat$x), length.out = 201)
  der <- t(vapply(seq_len(nrow(p)), function(i) {
    y <- .bayes_curve(p[i, ], "P3", grid)
    top <- which.max(y)
    inz <- grid[y >= 0.9 * y[top]]
    c(y[top], grid[top], min(inz), max(inz))
  }, numeric(4)))
  p$MFO <- der[, 1]
  p$Fatmax <- der[, 2]
  p$zone_lower <- der[, 3]
  p$zone_upper <- der[, 4]
  p
}

.bayes_result <- function(fit, dat, model, sigma, conf, fatmin_prior = c(90, 14)) {
  post <- .bayes_draws(fit, model, dat, "b_", fatmin_prior)
  prior <- .bayes_draws(fit, model, dat, "prior_b_", fatmin_prior)
  a <- (1 - conf) / 2
  pars <- if (model == "SIN") c("M", "Fatmax", "Fatmin", "d", "s", "t") else
    c("b0", "b1", "b2", "b3")
  qs <- c("MFO", "Fatmax", if (model == "SIN") "Fatmin", "zone_lower", "zone_upper",
          if (model == "SIN") c("d", "s", "t") else pars)
  summ <- data.frame(
    quantity = qs,
    median = vapply(qs, function(q) stats::median(post[[q]]), numeric(1)),
    lower = vapply(qs, function(q) stats::quantile(post[[q]], a, names = FALSE), numeric(1)),
    upper = vapply(qs, function(q) stats::quantile(post[[q]], 1 - a, names = FALSE), numeric(1)),
    contraction = vapply(qs, function(q) {
      pq <- if (q == "MFO" && model == "SIN") "M" else q
      if (!pq %in% pars) return(NA_real_)
      1 - stats::var(post[[pq]]) / stats::var(prior[[pq]])
    }, numeric(1)),
    row.names = qs, stringsAsFactors = FALSE)
  fx <- summary(fit)$fixed
  rh <- brms::rhat(fit)
  np <- brms::nuts_params(fit)
  cr <- if (model == "SIN") stats::cor(post[, c("d", "s", "t")]) else NULL
  if (!is.null(cr)) diag(cr) <- 0
  diag <- list(rhat_max = max(rh[!grepl("^prior_|^lp__|^lprior", names(rh))], na.rm = TRUE),
               ess_bulk_min = min(fx$Bulk_ESS), ess_tail_min = min(fx$Tail_ESS),
               divergent = as.integer(sum(np$Value[np$Parameter == "divergent__"])),
               max_abs_cor = if (is.null(cr)) NA_real_ else max(abs(cr)))
  if (diag$rhat_max > 1.01) {
    warning(sprintf("MFO_bayes(): max R-hat %.3f > 1.01; the chains have not mixed. ",
                    diag$rhat_max), "Increase iter or check the data.", call. = FALSE)
  }
  structure(list(summary = summ, draws = post, diagnostics = diag, model = model,
                 sigma = sigma, conf = conf, data = dat, fit = fit),
            class = "MFO_bayes")
}
