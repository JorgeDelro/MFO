
#' Maximal Fat Oxidation Kinetics (SIN Model)
#'
#' Fit the SIN model (Cheneviere et al., 2009) to describe fat oxidation kinetics
#' and extract three parameters: dilatation (d), symmetry (s), and translation (t).
#' Also calculates Fatmin (the intensity where fat oxidation reaches zero),
#' which is a unique capability of the SIN model over polynomial methods.
#'
#' The model is equation \[14\] of Cheneviere et al. (2009, Med Sci Sports Exerc
#' 41: 1615-1625, p. 1618, \doi{10.1249/MSS.0b013e31819e2f91}):
#' \deqn{\%MFO = \sin\{[\pi^{1/s} / (\pi + 2d) \cdot (Kx + d + t)]^s\},
#'       \quad K = \pi/100,}
#' x in \% VO2max. Up to v0.2.0 the package computed (pi^(1/s)/pi) + 2d, so its
#' `d` (and `t`) are not comparable with published values (SPEC 6.5, 19).
#'
#' @section Fitting procedure:
#' As in Cheneviere et al. (2009), the model is fitted by non-linear least squares
#' (Levenberg-Marquardt, [minpack.lm::nlsLM()], start d = 0, s = 1, t = 0)
#' directly to the observed rows of `MFO_data`, with fat oxidation expressed as a
#' proportion of the highest observed value (\%MFO / 100). There is no smoothing
#' and no synthetic grid: v0.1.0/v0.2.0 fitted the SIN to a fine grid predicted by
#' a third-degree polynomial, which inflated n and shrank the standard errors of
#' d, s and t (SPEC 19, finding 7). R-squared and RMSE are computed on the
#' observed proportions. At least 4 rows are needed for 3 parameters: with
#' fewer the function warns and returns `convergence = FALSE`; with 4 or 5 it
#' fits but warns that the fit is unreliable and that at least 6 steps are
#' recommended (SPEC 6.5).
#'
#' `FatMax_SIN` and `Fatmin` come from closed forms derived from eq. \[14\]:
#' Fatmax = \[(pi + 2d) 2^(-1/s) - d - t\] / K and, by default, Fatmin = second
#' zero of the curve = (pi + d - t) / K, Cheneviere's definition (SPEC 19,
#' finding 3). Fatmin can lie beyond the last observed step (extrapolation).
#'
#' @section Which rows are fitted:
#' `MFO_kinetics()` fits every row it receives. Cheneviere et al. (2009)
#' fitted exercise steps only, and averaged only steps with RER <= 1.0:
#' \itemize{
#' \item `MFO(..., RER_cutoff = 1)` reproduces their step selection;
#' \item `MFO(..., include_basal = TRUE)` adds the resting row to `MFO_db`, so it
#'   is also fitted here. This departs from Cheneviere's procedure in the SIN as
#'   well as in the P3.
#' }
#' The SIN is sensitive to the last steps because the curve must reach zero:
#' in the example data (`author = "Frayn"`) the last step (74.7 \% VO2max,
#' RER 1.03) has negative fat oxidation.
#' \itemize{
#' \item `negative_FAT = "truncate"` (default) sets it to 0 and that point anchors
#'   the second zero: d = -0.385, s = 0.910, t = 0.355, FatMax_SIN = 36.2,
#'   Fatmin = 76.4 \% VO2max.
#' \item `negative_FAT = "exclude"` or `RER_cutoff = 1` drops it: d = -0.440,
#'   s = 0.539, t = 0.023, FatMax_SIN = 33.2, Fatmin = 85.3 \% VO2max.
#' }
#' Report which option was used when publishing SIN parameters.
#'
#' @section Normalised or absolute fit:
#' With `fit = "normalised"` (default, Cheneviere et al. 2009) fat oxidation
#' is divided by the highest observed step before fitting. Unless a step falls
#' exactly on the true Fatmax, that step is below the true MFO and the shape
#' is distorted even without noise: in noiseless simulated tests with Fatmax
#' between two steps, d was 0.234 instead of 0.18 (SPEC 13.2, issue #13).
#' `fit = "absolute"` fits fat oxidation in g/min, FAT = M x SIN, with M (the
#' MFO of the curve, returned as `MFO_SIN`) as a fourth parameter, and has no
#' such bias: in noiseless tests it recovered d, s, t, Fatmax, Fatmin and MFO
#' to 1e-6 whether or not a step fell on Fatmax. It needs at least 5 steps.
#' With noise it removes the bias but not the error: with a FAT error of 5 \%
#' of MFO (200 simulated tests, trained shape), the median error of d went
#' from -0.15 (normalised) to +0.005 (absolute) and that of Fatmin from -0.29
#' to -0.02 \% VO2max, with similar spread (median absolute error of d 0.35
#' and 0.36). d is poorly identified with noisy data in either fit.
#' It is started from the normalised estimates. The default is to be reviewed
#' at 0.3.0-beta.1.
#'
#' @section Fit uncertainty:
#' `ci = "parametric"` (also `TRUE`) or `"bootstrap"` gives percentile
#' intervals for d, s, t, `FatMax_SIN`, `Fatmin` and, with the absolute fit,
#' `MFO_SIN`, as in [MFO()]: replicates are the fitted values plus new errors,
#' refitted from the estimates. `"parametric"` draws sigma^2 ~
#' Inv-chi^2(n - p, s^2) and then normal errors (Gelman et al. 2013, ch. 14);
#' `"bootstrap"` resamples modified residuals r / sqrt(1 - h), with h the
#' leverage from the gradient J of the fit, diag(J (J'J)^-1 J') (Davison &
#' Hinkley 1997, 7.4). Replicates that do not converge are dropped and
#' counted.
#'
#' @param MFO_data a data frame obtained from the MFO function (must contain FAT and porc_VO2 columns).
#' @param fatmin_threshold `NULL` (default) for Fatmin at the second zero of the
#'        curve, or a proportion of MFO in (0, 1), e.g. 0.05, for the intensity
#'        above Fatmax where fat oxidation falls to that fraction of MFO
#'        (closed form: \[(pi + 2d) ((pi - asin(threshold)) / pi)^(1/s) - d - t\] / K).
#'        v0.2.0 used a fixed 5 \% threshold searched on a grid.
#' @param fit `"normalised"` (default) or `"absolute"` (see *Normalised or
#'        absolute fit*).
#' @param ci `"none"` (default), `"parametric"` (also `TRUE`) or `"bootstrap"`
#'        (see *Fit uncertainty*).
#' @param B number of replicates (default 2000).
#' @param conf confidence level (default 0.95).
#' @param seed optional integer for reproducible intervals; the random state
#'        of the session is restored afterwards.
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename mutate pull
#' @importFrom stats predict coef residuals complete.cases
#' @import ggplot2
#'
#' @return {Returns a list which contains:
#' \itemize{
#' \item MFO_kinetics_data: `MFO_data` plus normalized \%MFO (`porc_MFO`, 100 =
#'   highest observed FAT, or the fitted MFO with `fit = "absolute"`).
#' \item MFO_kinetics_plot: ggplot object with the observed \%MFO, the basic SIN and the fitted SIN.
#' \item d: dilatation parameter (width of the curve).
#' \item t: translation parameter (horizontal shift along intensity axis).
#' \item s: symmetry parameter (controls curve shape asymmetry; <1 left-skew, >1 right-skew).
#' \item Fatmin: exercise intensity (\% VO2max) at the second zero of the curve, or
#'   at `fatmin_threshold` of MFO if given.
#' \item FatMax_SIN: exercise intensity (\% VO2max) at peak fat oxidation from SIN model.
#' \item R_squared: R-squared of the SIN model fit (on the fitted scale:
#'   proportion of MFO, or g/min with `fit = "absolute"`).
#' \item RMSE: Root mean squared error of the SIN model fit (same scale).
#' \item d_se: standard error of dilatation parameter.
#' \item s_se: standard error of symmetry parameter.
#' \item t_se: standard error of translation parameter.
#' \item convergence: logical, whether the SIN model converged.
#' \item fit: `"normalised"` or `"absolute"`.
#' \item MFO_SIN, MFO_SIN_se: MFO of the fitted curve (g/min) and its standard
#'   error with `fit = "absolute"`; `NA` otherwise.
#' \item ci: with `ci` other than `"none"`, a data frame with estimate, lower
#'   and upper for each quantity; otherwise `NULL`.
#' \item bootstrap: with `ci`, a list with `type`, `B`, `kept`, `conf` and the
#'   `replicates`; otherwise `NULL`.
#' }}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read dfs
#' data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
#'
#' # Convert to data.frame
#' basal_df <- data.frame(basal_df)
#' MFO_df <- data.frame(MFO_df)
#' VO2max_df <- data.frame(VO2max_df)
#'
#' # Calculate MFO and Fatmax
#' result_MFO <- MFO(step_time = 20,
#'                  db_MFO = MFO_df,
#'                  db_basal = basal_df,
#'                  db_graded = VO2max_df,
#'                  cv_var = "RER",
#'                  author = "Frayn")
#'
#' # Calculate MFO Kinetics
#' result_kinetics <- MFO_kinetics(result_MFO$MFO_db)
#'
#' # Fatmin at 5 % of MFO instead of the second zero
#' MFO_kinetics(result_MFO$MFO_db, fatmin_threshold = 0.05)$Fatmin
#' }
#'
MFO_kinetics <- function(MFO_data, fatmin_threshold = NULL,
                         fit = c("normalised", "absolute"),
                         ci = c("none", "parametric", "bootstrap"),
                         B = 2000, conf = 0.95, seed = NULL) {

  fit <- match.arg(fit)
  if (isTRUE(ci)) ci <- "parametric"
  if (identical(ci, FALSE)) ci <- "none"
  ci <- match.arg(ci)
  if (ci != "none") {
    if (!is.numeric(B) || length(B) != 1 || is.na(B) || B < 2 || B != round(B)) {
      stop("B must be a whole number >= 2.", call. = FALSE)
    }
    if (!is.numeric(conf) || length(conf) != 1 || is.na(conf) || conf <= 0 || conf >= 1) {
      stop("conf must be a single number between 0 and 1.", call. = FALSE)
    }
  }

  if (!is.null(fatmin_threshold) &&
      !(is.numeric(fatmin_threshold) && length(fatmin_threshold) == 1 &&
        isTRUE(fatmin_threshold > 0 && fatmin_threshold < 1))) {
    stop("fatmin_threshold must be NULL or a single proportion of MFO in (0, 1), e.g. 0.05.")
  }

  # Normalize to %MFO with the highest observed value (SPEC 6.5)
  max_fat <- max(MFO_data$FAT, na.rm = TRUE)
  if (max_fat <= 0) {
    stop("Maximum FAT value is <= 0. Cannot normalize for SIN model.")
  }

  MFO_kinetics_data <- MFO_data
  MFO_kinetics_data$porc_MFO <- (MFO_data$FAT * 100) / max_fat

  # Observed rows only: no smoothing, no synthetic grid (SPEC 19, finding 7).
  # "normalised" fits FAT / highest observed FAT; "absolute" fits FAT in g/min
  # with MFO as a fourth parameter (SPEC 13.2), with no normalisation bias
  data_nls <- data.frame(porc_VO2_kinetics = MFO_data$porc_VO2,
                         porc_MFO_kinetics = MFO_data$FAT / max_fat,
                         FAT = MFO_data$FAT)
  data_nls <- data_nls[complete.cases(data_nls), , drop = FALSE]
  n_par <- if (fit == "absolute") 4 else 3

  # --- SIN model fitting ---
  # %MFO = sin([pi^(1/s) / (pi + 2d) * (K*porc_VO2 + d + t)]^s), K = pi/100
  # Cheneviere et al. (2009), eq. [14]; see .sin_model() (SPEC 6.5)

  convergence <- TRUE
  d <- NA; s <- NA; t_param <- NA; M <- NA
  d_se <- NA; s_se <- NA; t_se <- NA; M_se <- NA
  R_squared_sin <- NA; RMSE_sin <- NA
  mod_nls <- NULL

  if (nrow(data_nls) < n_par + 1) {
    warning("SIN model needs at least ", n_par + 1, " steps to fit ", n_par,
            " parameters; ", nrow(data_nls), " available.")
    convergence <- FALSE
  } else {
    if (nrow(data_nls) < 6) {
      warning("SIN model fitted to ", nrow(data_nls), " steps: the fit is possible ",
              "but unreliable; at least 6 steps are recommended (SPEC 6.5).")
    }
    tryCatch({
      mod_nls <- .fit_sin(data_nls, fit)

      # Extract parameters
      coefs <- .sin_coef_table(mod_nls)
      d <- coefs["d", "Estimate"]
      s <- coefs["s", "Estimate"]
      t_param <- coefs["t_param", "Estimate"]
      d_se <- coefs["d", "Std. Error"]
      s_se <- coefs["s", "Std. Error"]
      t_se <- coefs["t_param", "Std. Error"]
      if (fit == "absolute") {
        M <- coefs["M", "Estimate"]
        M_se <- coefs["M", "Std. Error"]
      }

      # Goodness of fit on the fitted scale (proportion of MFO or g/min)
      y_fit <- if (fit == "absolute") data_nls$FAT else data_nls$porc_MFO_kinetics
      resid_sin <- residuals(mod_nls)
      ss_res <- sum(resid_sin^2)
      ss_tot <- sum((y_fit - mean(y_fit))^2)
      R_squared_sin <- 1 - ss_res / ss_tot
      RMSE_sin <- sqrt(mean(resid_sin^2))

    }, error = function(e) {
      warning(paste("SIN model did not converge:", e$message))
      convergence <<- FALSE
    })
  }

  # Post-convergence quality check: if parameters stayed at initial values
  # or R-squared is negative, the fit is meaningless
  if (convergence) {
    at_initial <- (abs(d) < 1e-6 && abs(s - 1) < 1e-6 && abs(t_param) < 1e-6)
    poor_fit <- !is.na(R_squared_sin) && R_squared_sin < 0
    if (at_initial || poor_fit) {
      warning("SIN model converged to trivial solution (parameters at initial values or R-squared < 0). ",
              "This typically occurs with insufficient data points (< 6 stages) ",
              "or monotonically decreasing fat oxidation patterns.")
      convergence <- FALSE
    }
  }

  # --- FatMax and Fatmin: closed forms derived from eq. [14] (SPEC 6.5) ---
  FatMax_SIN <- NA
  Fatmin <- NA
  if (convergence) {
    FatMax_SIN <- .sin_fatmax(d, s, t_param)
    Fatmin <- .sin_fatmin(d, s, t_param, threshold = fatmin_threshold)
  }

  # With the absolute fit the curve is shown and stored as % of the fitted MFO
  if (fit == "absolute" && convergence) {
    MFO_kinetics_data$porc_MFO <- (MFO_data$FAT * 100) / M
    data_nls$porc_MFO_kinetics <- data_nls$FAT / M
  }

  # --- Uncertainty of the SIN fit (SPEC 13.1) ---
  ci_table <- NULL
  bootstrap <- NULL
  if (ci != "none" && convergence) {
    reps <- .with_seed(seed, .sin_bootstrap(mod_nls, data_nls, fit, ci, B, fatmin_threshold))
    if (nrow(reps) < B) {
      warning(sprintf("%d of %d bootstrap replicates of the SIN did not converge and were dropped.",
                      B - nrow(reps), B), call. = FALSE)
    }
    if (nrow(reps) > 0) {
      bounds <- .percentile_ci(reps, conf)
      est <- c(d = d, s = s, t = t_param, FatMax_SIN = FatMax_SIN, Fatmin = Fatmin,
               MFO_SIN = M)[colnames(reps)]
      ci_table <- data.frame(estimate = est, lower = bounds[, 1], upper = bounds[, 2],
                             row.names = colnames(reps))
      bootstrap <- list(type = ci, B = B, kept = nrow(reps), conf = conf, replicates = reps)
    }
  }

  # --- Basic SIN curve (reference: d=0, s=1, t=0) ---
  porc_VO2_100 <- seq(from = 0, to = 100, length.out = 100)
  plot_data <- data.frame(porc_VO2 = porc_VO2_100,
                          values = pmax(.sin_model(porc_VO2_100, d = 0, s = 1, t = 0), 0),
                          curve = "Basic SIN")

  # --- Fitted SIN curve, drawn between its two zeros ---
  if (convergence) {
    x_from <- max(0, -(d + t_param) / (pi / 100))
    x_to <- min(100, .sin_fatmin(d, s, t_param))
    x_fit <- seq(from = x_from, to = x_to, length.out = 100)
    y_fit <- .sin_model(x_fit, d, s, t_param)
    y_fit[is.nan(y_fit) | y_fit < 0] <- 0  # rounding at the zeros
    plot_data <- rbind(plot_data,
                       data.frame(porc_VO2 = x_fit, values = y_fit,
                                  curve = "Fitted SIN"))
  }
  plot_data$curve <- factor(plot_data$curve, levels = c("Basic SIN", "Fitted SIN"))
  observed <- data.frame(porc_VO2 = data_nls$porc_VO2_kinetics,
                         values = data_nls$porc_MFO_kinetics)
  observed <- observed[observed$values >= 0, , drop = FALSE]

  # --- Plot ---
  # Rule: no numeric annotation inside the panel; parameters go to the subtitle
  subtitle <- if (convergence) {
    parts <- sprintf("d = %.2f \u00b7 s = %.2f \u00b7 t = %.2f", d, s, t_param)
    if (!is.na(Fatmin)) {
      parts <- paste0(parts, sprintf(" \u00b7 Fatmin = %.1f %% VO2max", Fatmin))
    }
    if (!is.na(R_squared_sin)) {
      parts <- paste0(parts, sprintf(" \u00b7 R\u00b2 = %.3f", R_squared_sin))
    }
    parts
  } else {
    "SIN model not fitted (no convergence)"
  }

  MFO_kinetics_plot <- ggplot(plot_data, aes(x = porc_VO2, y = values,
                                              group = curve, colour = curve)) +
    geom_line(linewidth = 0.8) +
    geom_point(data = observed, aes(x = porc_VO2, y = values),
               inherit.aes = FALSE, colour = "black", size = 2) +
    scale_color_manual(values = c("Basic SIN" = "grey60", "Fitted SIN" = "darkorange")) +
    ylim(0, 1.01) +
    xlim(0, 100) +
    labs(subtitle = subtitle) +
    ylab("Fat oxidation (%MFO)") +
    xlab(expression("Exercise intensity (% " * VO[2*max] * ")")) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 12))

  # Fatmin line if inside the plotted range
  if (!is.na(Fatmin) && Fatmin <= 100) {
    MFO_kinetics_plot <- MFO_kinetics_plot +
      geom_vline(xintercept = Fatmin, linetype = "dashed", color = "red", linewidth = 0.4)
  }

  return(list(
    MFO_kinetics_data = MFO_kinetics_data,
    MFO_kinetics_plot = MFO_kinetics_plot,
    d = d,
    t = t_param,
    s = s,
    Fatmin = Fatmin,
    FatMax_SIN = FatMax_SIN,
    R_squared = R_squared_sin,
    RMSE = RMSE_sin,
    d_se = d_se,
    s_se = s_se,
    t_se = t_se,
    convergence = convergence,
    fit = fit,
    MFO_SIN = M,
    MFO_SIN_se = M_se,
    ci = ci_table,
    bootstrap = bootstrap
  ))
}


# SIN fit: "normalised" to the highest observed step (Cheneviere et al.
# 2009) with nlsLM, or "absolute" in g/min with MFO (M) as a fourth parameter
# with minpack.lm::nls.lm, started from the normalised estimates. The
# absolute fit does not go through nls(), whose set-up checks the rank of a
# numerical gradient at the start and failed on some R versions for
# well-conditioned problems (condition number about 100).
.fit_sin <- function(data_nls, fit, start = NULL) {
  if (fit == "absolute") return(.fit_sin_absolute(data_nls, start))
  if (is.null(start)) start <- list(d = 0, s = 1, t_param = 0)
  nlsLM(porc_MFO_kinetics ~ .sin_model(porc_VO2_kinetics, d, s, t_param),
        start = start, data = data_nls, control = list(maxiter = 200))
}

# Absolute SIN fit by Levenberg-Marquardt on the residuals. Returns a list
# with coefficients (M, d, s, t_param), residuals, fitted.values, se and the
# gradient J (central differences at the solution), so that coef(),
# residuals() and fitted() work on it.
.fit_sin_absolute <- function(data_nls, start = NULL) {
  x <- data_nls$porc_VO2_kinetics
  y <- data_nls$FAT
  f <- function(p) p[["M"]] * .sin_model(x, p[["d"]], p[["s"]], p[["t_param"]])
  resid <- function(p) {
    r <- y - f(p)
    r[!is.finite(r)] <- 1e3   # outside the support of the SIN: push away
    r
  }
  starts <- if (!is.null(start)) list(unlist(start)) else {
    norm <- tryCatch(stats::coef(.fit_sin(data_nls, "normalised")),
                     error = function(e) c(d = 0, s = 1, t_param = 0))
    m0 <- max(y)
    list(c(M = m0, d = norm[["d"]], s = norm[["s"]], t_param = norm[["t_param"]]),
         c(M = 1.01 * m0, d = norm[["d"]] + 0.01, s = norm[["s"]], t_param = norm[["t_param"]]),
         c(M = m0, d = 0.01, s = 1.05, t_param = 0.05))
  }
  for (p0 in starts) {
    fit <- tryCatch(minpack.lm::nls.lm(par = p0, fn = resid,
                                       control = minpack.lm::nls.lm.control(maxiter = 200)),
                    error = function(e) NULL)
    if (is.null(fit) || !fit$info %in% 1:4 || any(!is.finite(fit$par))) next
    p <- fit$par
    fitted_y <- f(p)
    if (any(!is.finite(fitted_y))) next
    J <- vapply(seq_along(p), function(j) {
      h <- 1e-6 * max(1, abs(p[[j]]))
      up <- p; dn <- p
      up[[j]] <- up[[j]] + h
      dn[[j]] <- dn[[j]] - h
      (f(up) - f(dn)) / (2 * h)
    }, numeric(length(y)))
    df <- length(y) - length(p)
    s2 <- sum((y - fitted_y)^2) / df
    XtX <- crossprod(J)
    se <- if (rcond(XtX) > 1e-12) sqrt(diag(s2 * solve(XtX))) else rep(NA_real_, length(p))
    names(se) <- names(p)
    return(list(coefficients = p, residuals = y - fitted_y, fitted.values = fitted_y,
                se = se, gradient = J))
  }
  stop("the absolute SIN fit did not converge from any start", call. = FALSE)
}

# Estimate and standard error of each parameter, from nls or the absolute fit
.sin_coef_table <- function(mod) {
  if (inherits(mod, "nls")) return(summary(mod)$coefficients[, 1:2, drop = FALSE])
  cbind(Estimate = mod$coefficients, "Std. Error" = mod$se)
}

# Replicates of the SIN fit: fitted values plus new errors, refitted from the
# estimates. "bootstrap": modified residuals r / sqrt(1 - h), centred, with
# h = diag(J (J'J)^-1 J'), J the gradient of the fit (Davison & Hinkley 1997,
# 7.4); "parametric": sigma^2 ~ Inv-chi^2(n - p, s^2), then normal errors
# (Gelman et al. 2013, ch. 14). Non-converged replicates are dropped.
.sin_bootstrap <- function(mod, data_nls, fit, type, B, fatmin_threshold) {
  fitted_y <- stats::fitted(mod)
  r <- stats::residuals(mod)
  n <- length(r)
  if (type == "parametric") {
    df <- n - length(stats::coef(mod))
    if (df < 1) stop("The bootstrap needs more steps than SIN parameters.", call. = FALSE)
    s2 <- sum(r^2) / df
    draw <- function() stats::rnorm(n, 0, sqrt(df * s2 / stats::rchisq(1, df)))
  } else {
    J <- if (inherits(mod, "nls")) mod$m$gradient() else mod$gradient
    h <- diag(J %*% solve(crossprod(J)) %*% t(J))
    if (any(h > 1 - 1e-8)) {
      stop("The bootstrap needs more steps than SIN parameters (leverage 1).", call. = FALSE)
    }
    m <- r / sqrt(1 - h)
    m <- m - mean(m)
    draw <- function() sample(m, replace = TRUE)
  }
  start <- as.list(stats::coef(mod))
  cols <- c("d", "s", "t", "FatMax_SIN", "Fatmin", if (fit == "absolute") "MFO_SIN")
  out <- matrix(NA_real_, B, length(cols), dimnames = list(NULL, cols))
  for (b in seq_len(B)) {
    db <- data_nls
    if (fit == "absolute") db$FAT <- fitted_y + draw() else db$porc_MFO_kinetics <- fitted_y + draw()
    cf <- tryCatch(stats::coef(.fit_sin(db, fit, start)), error = function(e) NULL)
    if (is.null(cf) || anyNA(cf)) next
    out[b, ] <- c(cf[["d"]], cf[["s"]], cf[["t_param"]],
                  .sin_fatmax(cf[["d"]], cf[["s"]], cf[["t_param"]]),
                  .sin_fatmin(cf[["d"]], cf[["s"]], cf[["t_param"]], threshold = fatmin_threshold),
                  if (fit == "absolute") cf[["M"]])
  }
  out[stats::complete.cases(out), , drop = FALSE]
}
