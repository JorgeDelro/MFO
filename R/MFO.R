
#' Maximal Fat Oxidation & FatMax Function
#'
#' Calculate Maximal Fat Oxidation (MFO), the exercise intensity that elicits
#' MFO (FatMax), the FatMax zone, and the crossover point. Supports multiple
#' curve-fitting methods (P3, P2, SIN, MRER) and stoichiometric equations
#' (Frayn, Jeukendrup, Peronnet).
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db_MFO database containing MFO test.
#' @param db_basal database containing basal test.
#' @param db_graded database containing incremental exercise test (optional if VO2max provided).
#'        VO2max is the highest mean VO2 over `vo2max_window` seconds of consecutive records.
#' @param cv_var variable to estimate coefficient of variation. Can be: "VO2", "VCO2" or "RER".
#' @param author author to estimate MFO. Can be: "Frayn", "Jeukendrup" or "Peronnet".
#' @param VO2max VO2max can be passed directly (ml/min) instead of using db_graded.
#'        If both are given, `VO2max` is used as is and `db_graded` is ignored with a warning.
#' @param method curve-fitting method: "P3" (3rd-degree polynomial, default), "P2" (2nd-degree polynomial),
#'        or "MRER" (RER-based method of Perez-Martin et al.).
#' @param step_duration duration of each exercise step in seconds (default 180 = 3 min).
#' @param steady_state_duration duration of steady-state period to average within each step, in seconds (default 60).
#' @param RER_cutoff RER value above which steps are excluded (default NULL = no cutoff).
#'        Recommended values: 1.0 (general), 0.93 (sedentary/obese), 0.90 (trained).
#' @param negative_FAT how to handle negative fat oxidation values: "truncate" (set to 0, default),
#'        "exclude" (remove from fitting), or "keep" (include in fitting).
#' @param body_mass body mass in kg (optional, for relative MFO calculation).
#' @param fat_free_mass fat-free mass in kg (optional, for FFM-relative MFO calculation).
#' @param HRmax maximum heart rate in bpm (optional, for FatMax as \%HRmax).
#' @param vo2max_window averaging window in seconds for VO2max from `db_graded` (default 30).
#'        If it is not a multiple of `step_time`, the number of records is rounded up
#'        (30 s with 20-s records uses 2 records = 40 s). See SPEC 9.5 and 14.
#'
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom stats lm na.omit sd predict coef uniroot optimize approx
#'
#'
#' @return {Returns a list which contains:
#' \itemize{
#' \item MFO_db: database with Load, VO2, VCO2, HR, RER, CHO, FAT, Kcal, porc_VO2.
#' \item MFO_plot: ggplot object with the MFO plot including FAT and CHO curves.
#' \item MFO: Maximal fat oxidation (g/min).
#' \item MFO_relative: MFO relative to body mass (mg/kg/min), if body_mass provided.
#' \item MFO_FFM: MFO relative to fat-free mass (mg/kgFFM/min), if fat_free_mass provided.
#' \item FAT_MAX: Intensity that elicits MFO (\% VO2max).
#' \item FAT_MAX_HR: Heart rate at FatMax (bpm), interpolated from data.
#' \item FAT_MAX_porcHRmax: FatMax as \% HRmax, if HRmax provided.
#' \item FatMax_zone_lower: Lower bound of FatMax zone (\% VO2max).
#' \item FatMax_zone_upper: Upper bound of FatMax zone (\% VO2max).
#' \item crossover_point: Intensity at which CHO > FAT (\% VO2max).
#' \item R_squared: R-squared of the fitted model.
#' \item RMSE: Root mean squared error of the fitted model.
#' \item method: Method used for curve fitting.
#' \item VO2max: VO2max used to express intensity (ml/min).
#' \item VO2max_window: effective averaging window for VO2max (s); NA if VO2max was supplied.
#' \item VO2max_window_requested: `vo2max_window` as requested (s); NA if VO2max was supplied.
#' \item x_CHO: carbohydrates in basal metabolism (g/min).
#' \item x_FAT: fat in basal metabolism (g/min).
#' \item x_Kcal: Kcal in basal metabolism (kcal/min).
#' }}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Read dfs
#' data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
#'
#' # Convert to data.frame
#' basal_df <- data.frame(basal_df)
#' MFO_df <- data.frame(MFO_df)
#' VO2max_df <- data.frame(VO2max_df)
#'
#' # Calculate MFO and Fatmax (P3 method)
#' result_MFO <- MFO(step_time = 20,
#'                  db_MFO = MFO_df,
#'                  db_basal = basal_df,
#'                  db_graded = VO2max_df,
#'                  cv_var = "RER",
#'                  author = "Frayn",
#'                  method = "P3")
#'
#' # With all options
#' result_MFO <- MFO(step_time = 20,
#'                  db_MFO = MFO_df,
#'                  db_basal = basal_df,
#'                  db_graded = VO2max_df,
#'                  cv_var = "RER",
#'                  author = "Frayn",
#'                  method = "P3",
#'                  RER_cutoff = 1.0,
#'                  body_mass = 75,
#'                  fat_free_mass = 60,
#'                  HRmax = 190)
#' }
#'
MFO <- function(step_time,
                db_MFO,
                db_basal,
                db_graded = NULL,
                cv_var,
                author,
                VO2max = NULL,
                method = "P3",
                step_duration = 180,
                steady_state_duration = 60,
                RER_cutoff = NULL,
                negative_FAT = "truncate",
                body_mass = NULL,
                fat_free_mass = NULL,
                HRmax = NULL,
                vo2max_window = 30) {

  # Validate inputs
  method <- match.arg(method, c("P3", "P2", "MRER"))
  negative_FAT <- match.arg(negative_FAT, c("truncate", "exclude", "keep"))

  # Solve error: 'list' object cannot be coerced to type 'double'
  db_basal <- data.frame(db_basal)
  db_MFO <- data.frame(db_MFO)

  # Get VO2max: a value supplied by the lab wins over db_graded (SPEC 14);
  # otherwise the highest rolling mean over vo2max_window s (SPEC 9.5)
  VO2max_window <- NA
  VO2max_window_requested <- NA
  if (!is.null(VO2max)) {
    if (!is.null(db_graded)) {
      warning("Both VO2max and db_graded supplied; using VO2max and ignoring db_graded.")
    }
  } else if (!is.null(db_graded)) {
    db_graded <- data.frame(db_graded)
    vo2max_est <- .vo2max_rolling(db_graded[["VO2"]], step_time = step_time,
                                  window = vo2max_window)
    VO2max <- vo2max_est$VO2max
    VO2max_window <- vo2max_est$window
    VO2max_window_requested <- vo2max_window
  } else {
    stop("You must provide a VO2max value using db_graded or VO2max arguments.")
  }

  # Get basal data
  basal <- met_basal(step_time = step_time,
                     db = db_basal,
                     cv_var = cv_var)

  # Calculate CHO, FAT and Kcal for each step
  MFO_test <- calculate_vars(step_time = step_time,
                             db_MFO = db_MFO,
                             VO2max = VO2max,
                             author = author,
                             step_duration = step_duration,
                             steady_state_duration = steady_state_duration,
                             RER_cutoff = RER_cutoff)

  # Build database with basal + exercise steps
  n_steps <- nrow(MFO_test)
  Load <- c("basal", seq_len(n_steps) * (step_duration / 60) * 5) # approximate load labels

  VO2_vec <- c(basal$x_VO2, MFO_test$VO2)
  VCO2_vec <- c(mean(basal$db_5min$VCO2), MFO_test$VCO2)
  HR_vec <- c(NA, MFO_test$HR)
  RER_vec <- c(basal$x_RER, MFO_test$RER)
  FAT_vec <- c(basal$x_FAT, MFO_test$FAT)
  CHO_vec <- c(basal$x_CHO, MFO_test$CHO)
  Kcal_vec <- c(basal$x_Kcal, MFO_test$Kcal)

  MFO_db <- data.frame(
    Load = Load,
    VO2 = VO2_vec,
    VCO2 = VCO2_vec,
    HR = HR_vec,
    RER = RER_vec,
    CHO = CHO_vec,
    FAT = FAT_vec,
    Kcal = Kcal_vec,
    stringsAsFactors = FALSE
  )
  MFO_db$porc_VO2 <- (MFO_db$VO2 * 100) / VO2max

  # Remove NA's
  MFO_db <- na.omit(MFO_db)

  # Handle negative FAT values
  n_negative <- sum(MFO_db$FAT < 0)
  if (n_negative > 0) {
    warning(sprintf("%d step(s) with negative fat oxidation detected (RER > 1.0 artifact).", n_negative))
    if (negative_FAT == "truncate") {
      MFO_db$FAT[MFO_db$FAT < 0] <- 0
    } else if (negative_FAT == "exclude") {
      MFO_db <- MFO_db[MFO_db$FAT >= 0, , drop = FALSE]
    }
    # "keep" does nothing
  }

  # --- Curve fitting ---
  if (method == "P3") {
    result <- .fit_P3(MFO_db)
  } else if (method == "P2") {
    result <- .fit_P2(MFO_db)
  } else if (method == "MRER") {
    result <- .fit_MRER(MFO_db)
  }

  VAR_MFO <- result$MFO
  VO2_MFO <- result$FAT_MAX
  R_squared <- result$R_squared
  RMSE <- result$RMSE
  fat_predict_fn <- result$fat_predict_fn
  cho_predict_fn <- result$cho_predict_fn

  # --- FatMax zone (intensities within 10% of MFO) ---
  fatmax_zone <- .calculate_fatmax_zone(fat_predict_fn, VAR_MFO, VO2_MFO, MFO_db)

  # --- Crossover point (CHO > FAT) ---
  crossover <- .calculate_crossover(MFO_db)

  # --- HR at FatMax (interpolation) ---
  FAT_MAX_HR <- NA
  FAT_MAX_porcHRmax <- NA
  hr_data <- MFO_db[!is.na(MFO_db$HR), ]
  if (nrow(hr_data) >= 2) {
    hr_interp <- approx(hr_data$porc_VO2, hr_data$HR, xout = VO2_MFO)
    FAT_MAX_HR <- hr_interp$y
    if (!is.null(HRmax) && !is.na(FAT_MAX_HR)) {
      FAT_MAX_porcHRmax <- (FAT_MAX_HR / HRmax) * 100
    }
  }

  # --- Relative MFO ---
  MFO_relative <- NA
  MFO_FFM <- NA
  if (!is.null(body_mass)) {
    MFO_relative <- (VAR_MFO * 1000) / body_mass  # mg/kg/min
  }
  if (!is.null(fat_free_mass)) {
    MFO_FFM <- (VAR_MFO * 1000) / fat_free_mass  # mg/kgFFM/min
  }

  # --- Plot ---
  MFO_plot <- .create_MFO_plot(MFO_db, fat_predict_fn, cho_predict_fn,
                               VO2_MFO, VAR_MFO, fatmax_zone, crossover, method)

  return(list(MFO_db = MFO_db,
              MFO_plot = MFO_plot,
              MFO = VAR_MFO,
              MFO_relative = MFO_relative,
              MFO_FFM = MFO_FFM,
              FAT_MAX = VO2_MFO,
              FAT_MAX_HR = FAT_MAX_HR,
              FAT_MAX_porcHRmax = FAT_MAX_porcHRmax,
              FatMax_zone_lower = fatmax_zone$lower,
              FatMax_zone_upper = fatmax_zone$upper,
              crossover_point = crossover,
              R_squared = R_squared,
              RMSE = RMSE,
              method = method,
              VO2max = VO2max,
              VO2max_window = VO2max_window,
              VO2max_window_requested = VO2max_window_requested,
              x_CHO = basal$x_CHO,
              x_FAT = basal$x_FAT,
              x_Kcal = basal$x_Kcal))
}


# --- Internal helper: Fit P3 (3rd-degree polynomial) ---
.fit_P3 <- function(MFO_db) {
  mod <- lm(FAT ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3), data = MFO_db)
  coeffs <- coef(mod)

  # FAT = a + b*x + c*x^2 + d*x^3
  a <- coeffs[1]; b <- coeffs[2]; c <- coeffs[3]; d <- coeffs[4]

  # Check for NA coefficients (can happen with insufficient data)
  if (any(is.na(c(a, b, c, d)))) {
    warning("P3 model has NA coefficients. Falling back to P2.")
    return(.fit_P2(MFO_db))
  }

  # Derivative: b + 2c*x + 3d*x^2 = 0
  # Solve quadratic: 3d*x^2 + 2c*x + b = 0
  discriminant <- (2 * c)^2 - 4 * (3 * d) * b

  if (is.na(discriminant) || discriminant < 0) {
    # No real roots - fall back to P2
    warning("P3 model has no real critical points. Falling back to P2.")
    return(.fit_P2(MFO_db))
  }

  x1 <- (-2 * c + sqrt(discriminant)) / (2 * 3 * d)
  x2 <- (-2 * c - sqrt(discriminant)) / (2 * 3 * d)

  # Select the root that gives the maximum (not minimum) and is within data range
  fat_at_x1 <- a + b * x1 + c * x1^2 + d * x1^3
  fat_at_x2 <- a + b * x2 + c * x2^2 + d * x2^3

  x_range <- range(MFO_db$porc_VO2)
  candidates <- data.frame(x = c(x1, x2), fat = c(fat_at_x1, fat_at_x2))
  candidates <- candidates[candidates$x >= x_range[1] & candidates$x <= x_range[2], ]

  if (nrow(candidates) == 0) {
    warning("P3 critical points outside data range. Using point with highest observed FAT.")
    VO2_MFO <- MFO_db$porc_VO2[which.max(MFO_db$FAT)]
    VAR_MFO <- max(MFO_db$FAT)
  } else {
    best <- which.max(candidates$fat)
    VO2_MFO <- candidates$x[best]
    VAR_MFO <- candidates$fat[best]
  }

  # Guard: if polynomial predicts negative MFO, use observed max
  if (VAR_MFO < 0) {
    VO2_MFO <- MFO_db$porc_VO2[which.max(MFO_db$FAT)]
    VAR_MFO <- max(MFO_db$FAT)
  }

  # Goodness of fit
  ss_res <- sum(residuals(mod)^2)
  ss_tot <- sum((MFO_db$FAT - mean(MFO_db$FAT))^2)
  R_squared <- 1 - ss_res / ss_tot
  RMSE <- sqrt(mean(residuals(mod)^2))

  # Prediction functions
  fat_predict_fn <- function(x) {
    predict(mod, newdata = data.frame(porc_VO2 = x))
  }

  # Fit CHO model too for crossover and plot
  mod_cho <- lm(CHO ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3), data = MFO_db)
  cho_predict_fn <- function(x) {
    predict(mod_cho, newdata = data.frame(porc_VO2 = x))
  }

  return(list(MFO = VAR_MFO, FAT_MAX = VO2_MFO,
              R_squared = R_squared, RMSE = RMSE,
              fat_predict_fn = fat_predict_fn,
              cho_predict_fn = cho_predict_fn))
}


# --- Internal helper: Fit P2 (2nd-degree polynomial) ---
.fit_P2 <- function(MFO_db) {
  mod <- lm(FAT ~ porc_VO2 + I(porc_VO2^2), data = MFO_db)
  coeffs <- coef(mod)

  a <- coeffs[1]; b <- coeffs[2]; c <- coeffs[3]

  # Check for NA coefficients
  if (any(is.na(c(a, b, c)))) {
    VO2_MFO <- MFO_db$porc_VO2[which.max(MFO_db$FAT)]
    VAR_MFO <- max(MFO_db$FAT)
  } else {
    # Vertex of parabola: x = -b/(2c)
    VO2_MFO <- -b / (2 * c)
    VAR_MFO <- a + b * VO2_MFO + c * VO2_MFO^2
  }

  # Guard: if polynomial predicts negative MFO, use observed max
  if (VAR_MFO < 0) {
    VO2_MFO <- MFO_db$porc_VO2[which.max(MFO_db$FAT)]
    VAR_MFO <- max(MFO_db$FAT)
  }

  # Goodness of fit
  ss_res <- sum(residuals(mod)^2)
  ss_tot <- sum((MFO_db$FAT - mean(MFO_db$FAT))^2)
  R_squared <- 1 - ss_res / ss_tot
  RMSE <- sqrt(mean(residuals(mod)^2))

  fat_predict_fn <- function(x) {
    predict(mod, newdata = data.frame(porc_VO2 = x))
  }

  mod_cho <- lm(CHO ~ porc_VO2 + I(porc_VO2^2), data = MFO_db)
  cho_predict_fn <- function(x) {
    predict(mod_cho, newdata = data.frame(porc_VO2 = x))
  }

  return(list(MFO = VAR_MFO, FAT_MAX = VO2_MFO,
              R_squared = R_squared, RMSE = RMSE,
              fat_predict_fn = fat_predict_fn,
              cho_predict_fn = cho_predict_fn))
}


# --- Internal helper: Fit MRER method ---
.fit_MRER <- function(MFO_db) {
  # MRER: Fat ox = 1.7 * (1 - RER) * VO2 / 1000  (g/min)
  MFO_db$FAT_MRER <- fat_oxidation_MRER(MFO_db$VO2, MFO_db$RER)

  idx_max <- which.max(MFO_db$FAT_MRER)
  VAR_MFO <- MFO_db$FAT_MRER[idx_max]
  VO2_MFO <- MFO_db$porc_VO2[idx_max]

  # For MRER we use a simple polynomial fit for visualization
  mod <- lm(FAT_MRER ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3), data = MFO_db)

  ss_res <- sum(residuals(mod)^2)
  ss_tot <- sum((MFO_db$FAT_MRER - mean(MFO_db$FAT_MRER))^2)
  R_squared <- 1 - ss_res / ss_tot
  RMSE <- sqrt(mean(residuals(mod)^2))

  fat_predict_fn <- function(x) {
    predict(mod, newdata = data.frame(porc_VO2 = x))
  }

  mod_cho <- lm(CHO ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3), data = MFO_db)
  cho_predict_fn <- function(x) {
    predict(mod_cho, newdata = data.frame(porc_VO2 = x))
  }

  return(list(MFO = VAR_MFO, FAT_MAX = VO2_MFO,
              R_squared = R_squared, RMSE = RMSE,
              fat_predict_fn = fat_predict_fn,
              cho_predict_fn = cho_predict_fn))
}


# --- Internal helper: Calculate FatMax zone ---
.calculate_fatmax_zone <- function(fat_predict_fn, MFO_val, FatMax, MFO_db) {
  # FatMax zone: intensities where fat ox >= 90% of MFO (Achten et al., 2002)
  threshold <- MFO_val * 0.90
  x_range <- range(MFO_db$porc_VO2)
  x_seq <- seq(x_range[1], x_range[2], length.out = 1000)
  fat_pred <- fat_predict_fn(x_seq)

  above_threshold <- x_seq[fat_pred >= threshold]

  if (length(above_threshold) < 2) {
    return(list(lower = NA, upper = NA))
  }

  return(list(lower = min(above_threshold), upper = max(above_threshold)))
}


# --- Internal helper: Calculate crossover point ---
.calculate_crossover <- function(MFO_db) {
  # Crossover: intensity where CHO contribution exceeds FAT
  if (!"CHO" %in% colnames(MFO_db) || !"FAT" %in% colnames(MFO_db)) {
    return(NA)
  }

  diff_vals <- MFO_db$FAT - MFO_db$CHO

  # Find where diff changes sign (FAT > CHO to FAT < CHO)
  sign_changes <- which(diff(sign(diff_vals)) != 0)

  if (length(sign_changes) == 0) {
    return(NA)
  }

  # Linear interpolation at the first sign change
  i <- sign_changes[1]
  x1 <- MFO_db$porc_VO2[i]
  x2 <- MFO_db$porc_VO2[i + 1]
  y1 <- diff_vals[i]
  y2 <- diff_vals[i + 1]

  crossover <- x1 + (0 - y1) * (x2 - x1) / (y2 - y1)
  return(crossover)
}


# --- Internal helper: Create MFO plot ---
.create_MFO_plot <- function(MFO_db, fat_predict_fn, cho_predict_fn,
                              VO2_MFO, VAR_MFO, fatmax_zone, crossover, method) {

  x_seq <- seq(min(MFO_db$porc_VO2), max(MFO_db$porc_VO2), length.out = 200)
  fat_curve <- data.frame(porc_VO2 = x_seq, FAT = fat_predict_fn(x_seq))
  cho_curve <- data.frame(porc_VO2 = x_seq, CHO = cho_predict_fn(x_seq))

  VO2_MFO_text <- paste("FAT[MAX] == ~", round(VO2_MFO, 2), "*'% VO'[2]*'max'")
  VAR_MFO_text <- paste("MFO == ~", round(VAR_MFO, 3), "*' g/min'")
  method_text <- paste("Method:", method)

  p <- ggplot() +
    # FatMax zone shading
    {if (!is.na(fatmax_zone$lower) && !is.na(fatmax_zone$upper))
      annotate("rect",
               xmin = fatmax_zone$lower, xmax = fatmax_zone$upper,
               ymin = -Inf, ymax = Inf,
               alpha = 0.15, fill = "darkorange")
    } +
    # CHO curve
    geom_line(data = cho_curve, aes(x = porc_VO2, y = CHO),
              color = "steelblue", linewidth = 0.8, linetype = "dashed") +
    # FAT curve
    geom_line(data = fat_curve, aes(x = porc_VO2, y = FAT),
              color = "darkorange", linewidth = 1) +
    # Data points FAT
    geom_point(data = MFO_db, aes(x = porc_VO2, y = FAT),
               color = "darkorange", size = 3) +
    # Data points CHO
    geom_point(data = MFO_db, aes(x = porc_VO2, y = CHO),
               color = "steelblue", size = 2, shape = 17) +
    # FatMax vertical line
    geom_vline(xintercept = VO2_MFO, linetype = "dotted", color = "red", linewidth = 0.5) +
    # Crossover vertical line
    {if (!is.na(crossover))
      geom_vline(xintercept = crossover, linetype = "dotdash", color = "grey40", linewidth = 0.5)
    } +
    ylab(expression("Substrate oxidation (g" %.% "min"^{-1} * ")")) +
    xlab(expression("Exercise intensity (% " * VO[2*max] * ")")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 12)) +
    annotate("label",
             x = Inf, y = Inf,
             vjust = 1.5, hjust = 1.1,
             label = VO2_MFO_text,
             parse = TRUE, size = 3.5) +
    annotate("label",
             x = Inf, y = Inf,
             vjust = 3.2, hjust = 1.1,
             label = VAR_MFO_text,
             parse = TRUE, size = 3.5) +
    annotate("text",
             x = Inf, y = Inf,
             vjust = 5.5, hjust = 1.1,
             label = method_text, size = 3, color = "grey40") +
    # Legend labels
    annotate("text", x = max(MFO_db$porc_VO2) * 0.85, y = max(MFO_db$FAT, na.rm = TRUE) * 0.15,
             label = "FAT", color = "darkorange", fontface = "bold", size = 3.5) +
    annotate("text", x = max(MFO_db$porc_VO2) * 0.85, y = max(MFO_db$CHO, na.rm = TRUE) * 0.95,
             label = "CHO", color = "steelblue", fontface = "bold", size = 3.5)

  return(p)
}
