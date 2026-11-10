
#' Maximal Fat Oxidation Kinetics (SIN Model)
#'
#' Fit the SIN model (Cheneviere et al., 2009) to describe fat oxidation kinetics
#' and extract three parameters: dilatation (d), symmetry (s), and translation (t).
#' Also calculates Fatmin (the intensity where fat oxidation becomes negligible),
#' which is a unique capability of the SIN model over polynomial methods.
#'
#' The model is equation \[14\] of Cheneviere et al. (2009, Med Sci Sports Exerc
#' 41: 1615-1625, p. 1618, \doi{10.1249/MSS.0b013e31819e2f91}):
#' \deqn{\%MFO = \sin\{[\pi^{1/s} / (\pi + 2d) \cdot (Kx + d + t)]^s\},
#'       \quad K = \pi/100,}
#' x in \% VO2max. Up to v0.2.0 the package computed (pi^(1/s)/pi) + 2d, so its
#' `d` (and `t`) are not comparable with published values (SPEC 6.5, 19).
#'
#' @param MFO_data a data frame obtained from the MFO function (must contain FAT and porc_VO2 columns).
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename mutate pull
#' @importFrom stats predict coef residuals
#' @import ggplot2
#'
#' @return {Returns a list which contains:
#' \itemize{
#' \item MFO_kinetics_data: database used to create the MFO kinetics plot (original data plus normalized \%MFO).
#' \item MFO_kinetics_plot: ggplot object comparing basic and fitted SIN models.
#' \item d: dilatation parameter (controls vertical scaling/width of the curve).
#' \item t: translation parameter (horizontal shift along intensity axis).
#' \item s: symmetry parameter (controls curve shape asymmetry; <1 left-skew, >1 right-skew).
#' \item Fatmin: exercise intensity (\% VO2max) at which fat oxidation becomes negligible.
#' \item FatMax_SIN: exercise intensity (\% VO2max) at peak fat oxidation from SIN model.
#' \item R_squared: R-squared of the SIN model fit.
#' \item RMSE: Root mean squared error of the SIN model fit.
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
#' }
#'
MFO_kinetics <- function(MFO_data) {

  # Normalize to %MFO
  max_fat <- max(MFO_data$FAT, na.rm = TRUE)
  if (max_fat <= 0) {
    stop("Maximum FAT value is <= 0. Cannot normalize for SIN model.")
  }

  MFO_kinetics_data <- MFO_data
  MFO_kinetics_data$porc_MFO <- (MFO_data$FAT * 100) / max_fat

  # Fit cubic polynomial for smooth interpolation
  mod <- lm(porc_MFO ~ poly(porc_VO2, 3), data = MFO_kinetics_data)

  # Extract the last load step
  last_step <- as.numeric(MFO_data$porc_VO2[nrow(MFO_data)])

  # Create fine-grained data for SIN fitting
  n_points <- max(as.integer(last_step), 50)
  data_nls <- data.frame(porc_VO2 = seq(from = 0, to = last_step, length.out = n_points))

  # Get smoothed values and normalize to 0-1
  err <- predict(mod, newdata = data_nls, se.fit = FALSE)
  data_nls$porc_MFO_kinetics <- ((err * 100) / max(err)) / 100

  # Rename for SIN model fitting
  colnames(data_nls)[1] <- "porc_VO2_kinetics"

  # --- SIN model fitting ---
  # %MFO = sin([pi^(1/s) / (pi + 2d) * (K*porc_VO2 + d + t)]^s), K = pi/100
  # Cheneviere et al. (2009), eq. [14]; see .sin_model() (SPEC 6.5)

  convergence <- TRUE
  d <- NA; s <- NA; t_param <- NA
  d_se <- NA; s_se <- NA; t_se <- NA
  R_squared_sin <- NA; RMSE_sin <- NA

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

    # Goodness of fit
    resid_sin <- residuals(mod_nls)
    ss_res <- sum(resid_sin^2)
    ss_tot <- sum((data_nls$porc_MFO_kinetics - mean(data_nls$porc_MFO_kinetics))^2)
    R_squared_sin <- 1 - ss_res / ss_tot
    RMSE_sin <- sqrt(mean(resid_sin^2))

  }, error = function(e) {
    warning(paste("SIN model did not converge:", e$message))
    convergence <<- FALSE
  })

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

  # --- Calculate FatMax from SIN model ---
  FatMax_SIN <- NA
  if (convergence) {
    sin_fn <- function(x) .sin_model(x, d, s, t_param)
    # Find maximum by optimization
    opt <- optimize(sin_fn, interval = c(0, 100), maximum = TRUE)
    FatMax_SIN <- opt$maximum
  }

  # --- Calculate Fatmin (intensity where fat ox becomes negligible) ---
  Fatmin <- NA
  if (convergence && !is.na(FatMax_SIN)) {
    sin_fn <- function(x) .sin_model(x, d, s, t_param)
    # Fatmin: find where SIN curve crosses near 0 above FatMax
    tryCatch({
      x_search <- seq(FatMax_SIN, 100, length.out = 500)
      y_search <- sin_fn(x_search)
      # Find first crossing below a threshold (5% of max)
      threshold <- 0.05
      below <- which(y_search < threshold)
      if (length(below) > 0) {
        Fatmin <- x_search[below[1]]
      }
    }, error = function(e) {
      # Fatmin cannot be determined
    })
  }

  # --- Basic SIN curve (reference: d=0, s=1, t=0) ---
  porc_VO2_100 <- seq(from = 0, to = 100, length.out = 100)
  porc_MFO_basic <- .sin_model(porc_VO2_100, d = 0, s = 1, t = 0)

  # --- Fitted SIN curve ---
  if (convergence) {
    porc_MFO_fitted <- .sin_model(porc_VO2_100, d, s, t_param)
  } else {
    porc_MFO_fitted <- rep(NA, 100)
  }

  # --- Prepare plot data ---
  plot_data <- data.frame(
    porc_VO2 = rep(porc_VO2_100, 2),
    values = c(porc_MFO_basic, porc_MFO_fitted),
    curve = factor(rep(c("Basic SIN", "Fitted SIN"), each = 100),
                   levels = c("Basic SIN", "Fitted SIN"))
  )

  # --- Plot ---
  d_text <- paste("d == ~", round(d, 2))
  t_text <- paste("t == ~", round(t_param, 2))
  s_text <- paste("s == ~", round(s, 2))

  MFO_kinetics_plot <- ggplot(plot_data, aes(x = porc_VO2, y = values,
                                              group = curve, colour = curve)) +
    geom_line(linewidth = 0.8) +
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

  # Add Fatmin annotation if available
  if (!is.na(Fatmin)) {
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
