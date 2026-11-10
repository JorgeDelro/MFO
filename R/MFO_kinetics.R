
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
#' observed proportions. At least 4 rows are needed for 3 parameters.
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
#' @param MFO_data a data frame obtained from the MFO function (must contain FAT and porc_VO2 columns).
#' @param fatmin_threshold `NULL` (default) for Fatmin at the second zero of the
#'        curve, or a proportion of MFO in (0, 1), e.g. 0.05, for the intensity
#'        above Fatmax where fat oxidation falls to that fraction of MFO
#'        (closed form: \[(pi + 2d) ((pi - asin(threshold)) / pi)^(1/s) - d - t\] / K).
#'        v0.2.0 used a fixed 5 \% threshold searched on a grid.
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename mutate pull
#' @importFrom stats predict coef residuals complete.cases
#' @import ggplot2
#'
#' @return {Returns a list which contains:
#' \itemize{
#' \item MFO_kinetics_data: `MFO_data` plus normalized \%MFO (`porc_MFO`, 100 = highest observed FAT).
#' \item MFO_kinetics_plot: ggplot object with the observed \%MFO, the basic SIN and the fitted SIN.
#' \item d: dilatation parameter (width of the curve).
#' \item t: translation parameter (horizontal shift along intensity axis).
#' \item s: symmetry parameter (controls curve shape asymmetry; <1 left-skew, >1 right-skew).
#' \item Fatmin: exercise intensity (\% VO2max) at the second zero of the curve, or
#'   at `fatmin_threshold` of MFO if given.
#' \item FatMax_SIN: exercise intensity (\% VO2max) at peak fat oxidation from SIN model.
#' \item R_squared: R-squared of the SIN model fit (observed proportions of MFO).
#' \item RMSE: Root mean squared error of the SIN model fit (proportion of MFO).
#' \item d_se: standard error of dilatation parameter.
#' \item s_se: standard error of symmetry parameter.
#' \item t_se: standard error of translation parameter.
#' \item convergence: logical, whether the SIN model converged.
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
MFO_kinetics <- function(MFO_data, fatmin_threshold = NULL) {

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

  # Observed rows only: no smoothing, no synthetic grid (SPEC 19, finding 7)
  data_nls <- data.frame(porc_VO2_kinetics = MFO_data$porc_VO2,
                         porc_MFO_kinetics = MFO_data$FAT / max_fat)
  data_nls <- data_nls[complete.cases(data_nls), , drop = FALSE]

  # --- SIN model fitting ---
  # %MFO = sin([pi^(1/s) / (pi + 2d) * (K*porc_VO2 + d + t)]^s), K = pi/100
  # Cheneviere et al. (2009), eq. [14]; see .sin_model() (SPEC 6.5)

  convergence <- TRUE
  d <- NA; s <- NA; t_param <- NA
  d_se <- NA; s_se <- NA; t_se <- NA
  R_squared_sin <- NA; RMSE_sin <- NA

  if (nrow(data_nls) < 4) {
    warning("SIN model needs at least 4 steps to fit 3 parameters; ",
            nrow(data_nls), " available.")
    convergence <- FALSE
  } else {
    tryCatch({
      mod_nls <- nlsLM(
        porc_MFO_kinetics ~ .sin_model(porc_VO2_kinetics, d, s, t_param),
        start = list(d = 0, s = 1, t_param = 0),
        data = data_nls,
        control = list(maxiter = 200)
      )

      # Extract parameters
      coefs <- summary(mod_nls)$coefficients
      d <- coefs["d", "Estimate"]
      s <- coefs["s", "Estimate"]
      t_param <- coefs["t_param", "Estimate"]
      d_se <- coefs["d", "Std. Error"]
      s_se <- coefs["s", "Std. Error"]
      t_se <- coefs["t_param", "Std. Error"]

      # Goodness of fit on the observed proportions of MFO
      resid_sin <- residuals(mod_nls)
      ss_res <- sum(resid_sin^2)
      ss_tot <- sum((data_nls$porc_MFO_kinetics - mean(data_nls$porc_MFO_kinetics))^2)
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
  d_text <- paste("d == ~", round(d, 2))
  t_text <- paste("t == ~", round(t_param, 2))
  s_text <- paste("s == ~", round(s, 2))

  MFO_kinetics_plot <- ggplot(plot_data, aes(x = porc_VO2, y = values,
                                              group = curve, colour = curve)) +
    geom_line(linewidth = 0.8) +
    geom_point(data = observed, aes(x = porc_VO2, y = values),
               inherit.aes = FALSE, colour = "black", size = 2) +
    scale_color_manual(values = c("Basic SIN" = "grey60", "Fitted SIN" = "darkorange")) +
    ylim(0, 1.01) +
    xlim(0, 100) +
    ylab("Fat oxidation (%MFO)") +
    xlab(expression("Exercise intensity (% " * VO[2*max] * ")")) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 12)) +
    annotate("label",
             x = 5, y = 0.95,
             label = d_text,
             parse = TRUE, hjust = 0, size = 3.5) +
    annotate("label",
             x = 5, y = 0.875,
             label = t_text,
             parse = TRUE, hjust = 0, size = 3.5) +
    annotate("label",
             x = 5, y = 0.80,
             label = s_text,
             parse = TRUE, hjust = 0, size = 3.5)

  # Add Fatmin annotation if available and inside the plotted range
  if (!is.na(Fatmin) && Fatmin <= 100) {
    fatmin_text <- paste("Fatmin == ~", round(Fatmin, 1), "*'% VO'[2]*'max'")
    MFO_kinetics_plot <- MFO_kinetics_plot +
      geom_vline(xintercept = Fatmin, linetype = "dashed", color = "red", linewidth = 0.4) +
      annotate("label",
               x = 5, y = 0.725,
               label = fatmin_text,
               parse = TRUE, hjust = 0, size = 3.5)
  }

  # Add R-squared annotation
  if (!is.na(R_squared_sin)) {
    r2_text <- paste("R^2 == ~", round(R_squared_sin, 3))
    MFO_kinetics_plot <- MFO_kinetics_plot +
      annotate("label",
               x = 5, y = 0.65,
               label = r2_text,
               parse = TRUE, hjust = 0, size = 3.5)
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
    convergence = convergence
  ))
}
