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
#' non-integer powers of negative numbers out of Stan (SPEC 13.2). Fatmax,
#' Fatmin and the Fatmax zone (FAT >= 90 \% of M) come from closed forms of
#' each posterior draw.
#'
#' **P3 model.** FAT ~ b0 + b1 z + b2 z^2 + b3 z^3 with z = (x - 50) / 25;
#' Fatmax, MFO and the zone are found on a grid of the measured intensities
#' for each draw.
#'
#' **Priors.** The defaults are the authors' weakly informative choices, not
#' estimates from the literature. SIN: M ~ Normal(0.45, 0.25) truncated at
#' 0, centred above the normative mean of sedentary adults (0.34 +/- 0.10
#' g/min; Amaro-Gahete, Sanchez-Delgado & Ruiz 2018, Front Physiol 9: 1460,
#' Table 1, \doi{10.3389/fphys.2018.01460}) with an inflated SD to cover
#' trained people; d ~ Normal(0, 0.3); s ~ LogNormal(0, 0.3); t ~ Normal(0,
#' 0.2). P3: b0 ~ Normal(0.3, 0.3), b1-b3 ~ Normal(0, 0.3). sigma ~
#' Exponential(10) (constant) or, with `sigma = "proportional"`,
#' sigma_i = sigma_100 x_i / 100 with log(sigma_100) ~ Normal(log(0.1), 1).
#' Replace them with `priors` (a `brmsprior` object, e.g. from
#' [brms::prior()]); check them with [MFO_prior_check()].
#'
#' **Which parameters the data inform.** With 6-10 noisy steps the data say
#' little about d (and often s and t): their posteriors stay close to the
#' priors. `contraction` = 1 - Var(posterior) / Var(prior) measures it for
#' each parameter; below 0.5 the parameter is labelled `prior-dominated` in
#' `print()` and should not be interpreted (see [MFO_kinetics()], *What to
#' interpret*). MFO, Fatmax and Fatmin are what the curve determines.
#'
#' @param MFO_data the `MFO_db` of an [MFO()] result (columns `porc_VO2` and
#'   `FAT`); every row is used, as in [MFO_kinetics()].
#' @param model `"SIN"` (default) or `"P3"`.
#' @param priors optional `brmsprior` object replacing all default priors.
#' @param sigma `"constant"` (default) or `"proportional"` to intensity.
#' @param chains,iter number of chains and iterations per chain (defaults 4
#'   and 2000, half of them warm-up).
#' @param conf level of the credible intervals (default 0.95).
#' @param seed optional integer passed to Stan.
#' @param backend passed to [brms::brm()] (default `"rstan"`).
#' @param adapt_delta target acceptance rate of the sampler (default 0.95;
#'   the SIN likelihood has kinks at the zeros of the curve).
#' @param ... further arguments to [brms::brm()].
#'
#' @return An object of class `MFO_bayes`: a list with `summary` (one row per
#'   quantity: `median`, `lower`, `upper`, `contraction` for parameters),
#'   `draws` (posterior draws of the parameters and derived quantities),
#'   `diagnostics` (maximum R-hat, minimum bulk and tail ESS, divergent
#'   transitions), `model`, `sigma`, `conf` and `fit` (the `brmsfit`, e.g. for
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
                      sigma = c("constant", "proportional"), chains = 4, iter = 2000,
                      conf = 0.95, seed = NULL, backend = "rstan", adapt_delta = 0.95,
                      ...) {
  model <- match.arg(model)
  sigma <- match.arg(sigma)
  .require_brms()
  if (!is.numeric(conf) || length(conf) != 1 || is.na(conf) || conf <= 0 || conf >= 1) {
    stop("conf must be a single number between 0 and 1.", call. = FALSE)
  }
  dat <- .bayes_data(MFO_data)
  fit <- brms::brm(.bayes_formula(model, sigma), data = dat,
                   prior = if (is.null(priors)) .bayes_priors(model, sigma) else priors,
                   sample_prior = "yes", chains = chains, iter = iter,
                   seed = if (is.null(seed)) NA else seed, backend = backend,
                   init = .bayes_init(model, chains), refresh = 0, silent = 2,
                   control = list(adapt_delta = adapt_delta), ...)
  .bayes_result(fit, dat, model, sigma, conf)
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
                            sigma = c("constant", "proportional"), ndraws = 200,
                            seed = NULL, backend = "rstan", ...) {
  model <- match.arg(model)
  sigma <- match.arg(sigma)
  .require_brms()
  dat <- .bayes_data(MFO_data)
  fit <- brms::brm(.bayes_formula(model, sigma), data = dat,
                   prior = if (is.null(priors)) .bayes_priors(model, sigma) else priors,
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
  subtitle <- paste0(ci("MFO", "MFO = %.3f (%.3f-%.3f) g/min"), " · ",
                     ci("Fatmax", "FATmax = %.1f (%.1f-%.1f) %% VO2max"),
                     if (x$model == "SIN") paste0(" · ", ci("Fatmin", "Fatmin = %.1f (%.1f-%.1f)")),
                     sprintf(" · %s, %g %% CrI", x$model, 100 * x$conf))
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
    f <- FAT ~ M * sin(fmin(fmax(pi()^(1 / s) / (pi() + 2 * d) * (pi() / 100 * x + d + t),
                                 0)^s, pi()))
    if (is.null(sig)) brms::bf(f, M + d + s + t ~ 1, nl = TRUE) else
      brms::bf(f, M + d + s + t ~ 1, sig, nl = TRUE)
  } else {
    if (is.null(sig)) brms::bf(FAT ~ 1 + z + z2 + z3) else brms::bf(FAT ~ 1 + z + z2 + z3, sig)
  }
}

# Authors' weakly informative choices (see ?MFO_bayes)
.bayes_priors <- function(model, sigma) {
  p <- if (model == "SIN") {
    c(brms::prior_string("normal(0.45, 0.25)", nlpar = "M", lb = 0),
      brms::prior_string("normal(0, 0.3)", nlpar = "d"),
      brms::prior_string("lognormal(0, 0.3)", nlpar = "s", lb = 0),
      brms::prior_string("normal(0, 0.2)", nlpar = "t"))
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

.bayes_init <- function(model, chains) {
  one <- if (model == "SIN") {
    list(b_M = as.array(0.4), b_d = as.array(0), b_s = as.array(1), b_t = as.array(0))
  } else {
    "random"
  }
  if (identical(one, "random")) return("random")
  lapply(seq_len(chains), function(i) one)
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
.bayes_draws <- function(fit, model, dat, prefix = "b_") {
  all <- as.data.frame(brms::as_draws_df(fit))
  if (model == "SIN") {
    nm <- c(M = "M_Intercept", d = "d_Intercept", s = "s_Intercept", t = "t_Intercept")
  } else {
    nm <- c(b0 = "Intercept", b1 = "z", b2 = "z2", b3 = "z3")
  }
  # brms names: posterior b_M_Intercept / b_Intercept, b_z...; prior draws
  # prior_b_M (SIN) and prior_Intercept, prior_b (P3, one prior for all b)
  cols <- if (prefix == "b_") paste0(prefix, nm) else if (model == "SIN") {
    paste0("prior_b_", names(nm))
  } else {
    c("prior_Intercept", rep("prior_b", 3))
  }
  p <- as.data.frame(lapply(stats::setNames(cols, names(nm)), function(cl) all[[cl]]))
  if (model == "SIN") {
    K <- pi / 100
    lev <- asin(0.9)
    p$MFO <- p$M
    p$Fatmax <- .sin_fatmax(p$d, p$s, p$t)
    p$Fatmin <- .sin_fatmin(p$d, p$s, p$t)
    p$zone_lower <- ((pi + 2 * p$d) * (lev / pi)^(1 / p$s) - p$d - p$t) / K
    p$zone_upper <- ((pi + 2 * p$d) * ((pi - lev) / pi)^(1 / p$s) - p$d - p$t) / K
  } else {
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
  }
  p
}

.bayes_result <- function(fit, dat, model, sigma, conf) {
  post <- .bayes_draws(fit, model, dat, "b_")
  prior <- .bayes_draws(fit, model, dat, "prior_b_")
  a <- (1 - conf) / 2
  pars <- if (model == "SIN") c("M", "d", "s", "t") else c("b0", "b1", "b2", "b3")
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
  diag <- list(rhat_max = max(rh[!grepl("^prior_|^lp__|^lprior", names(rh))], na.rm = TRUE),
               ess_bulk_min = min(fx$Bulk_ESS), ess_tail_min = min(fx$Tail_ESS),
               divergent = as.integer(sum(np$Value[np$Parameter == "divergent__"])))
  if (diag$rhat_max > 1.01) {
    warning(sprintf("MFO_bayes(): max R-hat %.3f > 1.01; the chains have not mixed. ",
                    diag$rhat_max), "Increase iter or check the data.", call. = FALSE)
  }
  structure(list(summary = summ, draws = post, diagnostics = diag, model = model,
                 sigma = sigma, conf = conf, data = dat, fit = fit),
            class = "MFO_bayes")
}
