
#' Basal metabolic rate
#'
#' Calculate basal metabolism from rest test data using a rolling window
#' to identify the most metabolically stable 5-minute period.
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db a data frame with columns VO2, VCO2, RER, HR.
#' @param cv_var variable to calculate coefficient of variation: "VO2", "VCO2" or "RER".
#' @param window_duration duration of the stable window in seconds (default 300 = 5 min).
#' @param author equation set used for resting substrate oxidation: "Frayn" (default),
#'        "Jeukendrup" or "Peronnet". `MFO()` passes its own `author`, so resting and
#'        exercise values use the same equations (SPEC 9.5). Jeukendrup & Wallis (2005)
#'        propose no resting equation; with "Jeukendrup" the package uses the 100 \% glucose
#'        pair derived from their Table 4 (see [substrate_oxidation()], `phase = "rest"`).
#' @param energy_equivalents "classic" (default) or "source"; see [substrate_oxidation()].
#'
#' @importFrom dplyr pull mutate
#' @importFrom tibble add_column
#'
#' @keywords internal
met_basal <- function(step_time, db, cv_var, window_duration = 300,
                      author = "Frayn", energy_equivalents = "classic") {

  # Number of records in the stable window
  window_size <- as.integer(window_duration / step_time)

  # Discard initial stabilization period (first ~5 min)
  stabilization_records <- as.integer(window_duration / step_time)

  if (nrow(db) < (stabilization_records + window_size)) {
    warning("Basal database has fewer rows than expected for stabilization + window. Using all data.")
    stabilization_records <- 0
  }

  # Subset after stabilization

  db_stable <- db[(stabilization_records + 1):nrow(db), , drop = FALSE]

  # Rolling window: calculate CV for each window of window_size records
  n_windows <- nrow(db_stable) - window_size + 1

  if (n_windows < 1) {
    warning("Not enough data after stabilization. Using all post-stabilization data.")
    db_5min <- db_stable
  } else {
    cv_values <- numeric(n_windows)

    for (i in seq_len(n_windows)) {
      window_data <- db_stable[i:(i + window_size - 1), cv_var, drop = TRUE]
      if (is.data.frame(window_data)) window_data <- window_data[[1]]
      cv_values[i] <- sd(window_data) / mean(window_data)
    }

    # Select the window with the lowest CV
    best_start <- which.min(cv_values)
    best_end <- best_start + window_size - 1
    db_5min <- db_stable[best_start:best_end, , drop = FALSE]
  }

  # Substrate oxidation for the stable window, same equations as the test
  db_5min_calc <- db_5min
  subs <- substrate_oxidation(VO2 = db_5min_calc$VO2, VCO2 = db_5min_calc$VCO2,
                              author = author, phase = "rest",
                              energy_equivalents = energy_equivalents)
  db_5min_calc$CHO <- subs$CHO
  db_5min_calc$FAT <- subs$FAT
  db_5min_calc$Kcal <- subs$Kcal

  # Return means
  x_VO2 <- mean(db_5min_calc$VO2)
  x_RER <- mean(db_5min_calc$RER)
  x_CHO <- mean(db_5min_calc$CHO)
  x_FAT <- mean(db_5min_calc$FAT)
  x_Kcal <- mean(db_5min_calc$Kcal)

  return(list(db = db,
              db_5min = db_5min_calc,
              x_VO2 = x_VO2,
              x_RER = x_RER,
              x_CHO = x_CHO,
              x_FAT = x_FAT,
              x_Kcal = x_Kcal))
}


#' Substrate oxidation equations
#'
#' Calculate fat and carbohydrate oxidation from VO2 and VCO2 using
#' different stoichiometric equations.
#'
#' @param VO2 oxygen consumption in ml/min (scalar or vector)
#' @param VCO2 carbon dioxide production in ml/min (same length as VO2)
#' @param author equation set: "Frayn", "Jeukendrup", or "Peronnet"
#' @param VO2max maximum oxygen uptake in ml/min (required for Jeukendrup during exercise)
#' @param phase "exercise" (default) or "rest". Only changes "Jeukendrup".
#' @param energy_equivalents kcal per g of CHO and FAT used for `Kcal`:
#'        "classic" (default, 4 and 9) or "source" (values of the equation's source,
#'        see Details).
#'
#' @details
#' Rule: a coefficient printed in the source is used as printed; a derived
#' coefficient is used only where the source prints no equation (SPEC 19).
#'
#' **Frayn (1983)** (J Appl Physiol 55: 628-634, eqs. 5 and 6, n = 0), as printed:
#' CHO = 4.55 VCO2 - 3.21 VO2, FAT = 1.67 VO2 - 1.67 VCO2. Deriving them from his
#' Table 1 constants (glucose 0.746/0.746, fat 2.03/1.43 L/g) gives
#' 4.535/3.195 and 1.667/1.667; this is only a cross-check in the tests.
#'
#' **Peronnet & Massicotte (1991)** (Can J Sport Sci 16: 23-29) print no
#' equations. The package derives them from the O2/CO2 constants of their
#' Table 1 (glucose 0.7455/0.7426, fatty acid 2.0092/1.4136 L/g) by solving
#' VO2 = 0.7455 CHO + 2.0092 FAT and VCO2 = 0.7426 CHO + 1.4136 FAT for CHO and
#' FAT: CHO = 4.58519 VCO2 - 3.22597 VO2, FAT = 1.69469 VO2 - 1.70130 VCO2.
#' Up to v0.2.0 the rounded 4.585 / 3.2255 / 1.6946 / 1.7012 were used
#' (SPEC 19, finding 6).
#'
#' **Jeukendrup & Wallis (2005)** (Int J Sports Med 26 Suppl 1: S28-S37), with the
#' coefficients printed in their Table 5:
#' \itemize{
#' \item below 50 \% VO2max (40-50 \% set): CHO = 4.344 VCO2 - 3.061 VO2;
#' \item at or above 50 \% VO2max (50-75 \% set): CHO = 4.210 VCO2 - 2.962 VO2;
#' \item FAT = 1.695 VO2 - 1.701 VCO2 in both.
#' }
#' With `phase = "rest"` there is no printed equation: Jeukendrup & Wallis propose
#' none. As a package decision (pure glucose, no glycogen use at rest), the pair is
#' derived from the O2/CO2 constants of their Table 4 (glucose 0.7455/0.7426,
#' fatty acid 2.0092/1.4136 L/g) by solving the 2x2 system:
#' CHO = 4.58519 VCO2 - 3.22597 VO2, FAT = 1.69469 VO2 - 1.70130 VCO2.
#' The same derivation with 50/50 and 20/80 glucose/glycogen reproduces the
#' Table 5 coefficients within 0.005 and is kept as a cross-check in the tests.
#'
#' The Table 5 sets are defined for 40-75 \% VO2max only. Below 40 \% the 40-50 \%
#' set and above 75 \% the 50-75 \% set are extrapolated; [calculate_vars()] warns
#' when an exercise step falls outside the range (SPEC 3, 9.5). Resting values
#' (`phase = "rest"`) never trigger that warning.
#'
#' **Energy equivalents** (`Kcal = CHO * e_cho + FAT * e_fat`, SPEC 19, finding 1):
#' \itemize{
#' \item "classic": 4 kcal/g CHO and 9 kcal/g FAT for every equation (v0.2.0).
#'   Default in 0.2.x; the default will change to "source" in 0.3.0.
#' \item "source", `author = "Peronnet"`: glucose 3.8683 and fat 9.7460 kcal/g
#'   (Peronnet & Massicotte 1991, Table 1).
#' \item "source", `author = "Frayn"`: Frayn (1983, p. 632) gives no energy
#'   equivalents; as a package decision the Peronnet & Massicotte values are used.
#' \item "source", `author = "Jeukendrup"`: Jeukendrup & Wallis (2005, Table 5),
#'   CHO 3.95 kcal/g below 50 \% VO2max (40-50 \% set) and 4.07 kcal/g at or above
#'   50 \% (50-75 \% set), fat 9.75 kcal/g. At rest (`phase = "rest"`, pure glucose
#'   pair, package decision) the Peronnet & Massicotte glucose and fat values are used,
#'   because they are the constants of that pair. `Kcal` jumps at 50 \% VO2max
#'   with the CHO value.
#' }
#'
#' @return Named list with CHO (g/min), FAT (g/min), Kcal (kcal/min)
#'
#' @keywords internal
substrate_oxidation <- function(VO2, VCO2, author, VO2max = NULL,
                                phase = c("exercise", "rest"),
                                energy_equivalents = c("classic", "source")) {

  phase <- match.arg(phase)
  energy_equivalents <- match.arg(energy_equivalents)

  # Convert to L/min for equations (inputs are in ml/min)
  vo2 <- VO2 / 1000
  vco2 <- VCO2 / 1000

  if (author == "Frayn") {
    # Frayn (1983) - J Appl Physiol
    CHO <- 4.55 * vco2 - 3.21 * vo2
    FAT <- 1.67 * vo2 - 1.67 * vco2

  } else if (author == "Jeukendrup") {
    # Jeukendrup & Wallis (2005) - Int J Sports Med
    if (phase == "rest") {
      # No published resting equation: 100 % glucose pair derived from Table 4
      k <- .jeukendrup_coefficients(glucose_fraction = 1)
      CHO <- k[["cho_VCO2"]] * vco2 - k[["cho_VO2"]] * vo2
      FAT <- k[["fat_VO2"]] * vo2 - k[["fat_VCO2"]] * vco2
    } else {
      # Coefficients as printed in Table 5
      if (is.null(VO2max)) stop("VO2max is required for Jeukendrup equations")
      pct_vo2 <- (VO2 / VO2max) * 100
      FAT <- 1.695 * vo2 - 1.701 * vco2
      # Vectorised: each value picks the 40-50 % or 50-75 % VO2max set
      CHO <- ifelse(pct_vo2 < 50,
                    4.344 * vco2 - 3.061 * vo2,
                    4.210 * vco2 - 2.962 * vo2)
    }

  } else if (author == "Peronnet") {
    # Peronnet & Massicotte (1991): no printed equation, derived from Table 1
    k <- .peronnet_coefficients()
    CHO <- k[["cho_VCO2"]] * vco2 - k[["cho_VO2"]] * vo2
    FAT <- k[["fat_VO2"]] * vo2 - k[["fat_VCO2"]] * vco2

  } else {
    stop("author must be 'Frayn', 'Jeukendrup', or 'Peronnet'")
  }

  e <- .energy_equivalents(energy_equivalents, author, phase,
                           pct_vo2 = if (is.null(VO2max)) NULL else VO2 / VO2max * 100)
  Kcal <- CHO * e$cho + FAT * e$fat

  return(list(CHO = CHO, FAT = FAT, Kcal = Kcal))
}


# kcal per g of CHO and FAT (see substrate_oxidation(), SPEC 19 finding 1)
# Peronnet & Massicotte (1991, Table 1): glucose 3.8683, fat 9.7460 kcal/g.
# Jeukendrup & Wallis (2005, Table 5): CHO 3.95 (40-50 %) / 4.07 (50-75 %), fat 9.75.
.energy_equivalents <- function(type, author, phase = "exercise", pct_vo2 = NULL) {
  if (type == "classic") return(list(cho = 4, fat = 9))
  if (author == "Jeukendrup" && phase == "exercise") {
    return(list(cho = ifelse(pct_vo2 < 50, 3.95, 4.07), fat = 9.75))
  }
  list(cho = 3.8683, fat = 9.7460)
}


# Session state: messages shown once per session
.mfo_state <- new.env(parent = emptyenv())

# The default energy_equivalents changes in 0.3.0; tell users who rely on it
.age_default_message <- function() {
  if (isTRUE(.mfo_state$age_message_shown)) return(invisible(FALSE))
  .mfo_state$age_message_shown <- TRUE
  message("VO2max_type: age not given, so the heart-rate criterion of VO2max is not ",
          "evaluated and VO2max is labelled from the plateau only. Set age to ",
          "evaluate it (shown once per session).")
  invisible(TRUE)
}

.energy_default_message <- function() {
  if (isTRUE(.mfo_state$energy_message_shown)) return(invisible(FALSE))
  .mfo_state$energy_message_shown <- TRUE
  message("Kcal uses energy_equivalents = \"classic\" (4 kcal/g CHO, 9 kcal/g FAT). ",
          "The default will change to \"source\" (values of the equation's source) ",
          "in MFO 0.3.0. Set energy_equivalents explicitly to silence this message ",
          "(shown once per session).")
  invisible(TRUE)
}


#' Fat oxidation with the RER simplification (MRER)
#'
#' FAT (mg/min) = 1.7 (1 - RER) VO2 (ml/min), the simplification of the
#' Peronnet & Massicotte (1991) equation used by Perez-Martin et al. (2001) and
#' Cheneviere et al. (2009, p. 1617). Used by `method = "MRER"` in [MFO()].
#'
#' @param VO2 vector of oxygen consumption values (ml/min)
#' @param RER vector of respiratory exchange ratio values
#'
#' @return fat oxidation rate in g/min
#'
#' @keywords internal
fat_oxidation_MRER <- function(VO2, RER) {
  # MRER method: Fat ox (mg/min) = 1.7 * (1 - RER) * VO2
  # VO2 in ml/min, result in mg/min -> convert to g/min
  fat_ox <- 1.7 * (1 - RER) * VO2 / 1000
  return(fat_ox)
}


#' Calculate step means from raw data
#'
#' Generalized function that works with any step_time and step_duration.
#' Extracts the last minute of each step (steady-state) for averaging.
#'
#' @param step_time how often the data was collected (in seconds).
#' @param step_duration duration of each exercise step in seconds (default 180 = 3 min).
#' @param steady_state_duration duration of steady-state period to average within each step, in seconds (default 60 = last minute).
#' @param db_MFO data frame with MFO test (columns: VO2, VCO2, RER, HR).
#' @param VO2max maximum oxygen uptake in ml/min.
#' @param author equation set: "Frayn", "Jeukendrup", or "Peronnet".
#' @param RER_cutoff RER value above which data is excluded (default NULL = no cutoff),
#'        or one of "classic", "safe", "sedentary", "trained" (see [MFO()]).
#' @param energy_equivalents "classic" (default) or "source"; see [substrate_oxidation()].
#' @param use_incomplete_step logical (default `FALSE`). See [MFO()].
#' @param protocol optional [MFO_protocol()]; if given, `step_duration` and
#'   `steady_state_duration` are not used.
#'
#' @details With `author = "Jeukendrup"`, a single warning lists the steps (after
#' `RER_cutoff`) whose intensity is outside 40-75 \% VO2max, the range of the
#' Jeukendrup & Wallis (2005, Table 5) equations (SPEC 3, 9.5).
#'
#' @importFrom dplyr pull mutate
#'
#' @return data frame with columns: Step (step number in the test), Load (mean of
#'   an optional `Load` column, else NA), VO2, VCO2, HR,
#'   RER, CHO, FAT, Kcal (one row per step kept)
#'
#' @keywords internal
calculate_vars <- function(step_time,
                           db_MFO,
                           VO2max,
                           author,
                           step_duration = 180,
                           steady_state_duration = 60,
                           RER_cutoff = NULL,
                           energy_equivalents = "classic",
                           use_incomplete_step = FALSE,
                           protocol = NULL) {

  if (is.null(protocol)) {
    protocol <- MFO_protocol("step", step_duration = step_duration,
                             steady_state_duration = steady_state_duration)
  }
  bounds <- .step_bounds(protocol, nrow(db_MFO), step_time)

  # Incomplete last step: always warn; use it only if asked and it holds
  # the steady-state segment (SPEC 9.5, "Escalones incompletos")
  last <- bounds[nrow(bounds), ]
  if (nrow(bounds) > 0 && !last$complete &&
      !.incomplete_last_step(last$available, last$records, last$steady_records,
                             step_time, use_incomplete_step)) {
    bounds <- bounds[-nrow(bounds), , drop = FALSE]
  }
  n_steps <- nrow(bounds)

  if (n_steps < 1) {
    stop("Not enough data for at least one complete step.")
  }

  db_vars <- data.frame(
    Step = bounds$step,
    Load = NA_real_,
    VO2 = numeric(n_steps),
    VCO2 = numeric(n_steps),
    HR = numeric(n_steps),
    RER = numeric(n_steps),
    CHO = numeric(n_steps),
    FAT = numeric(n_steps),
    Kcal = numeric(n_steps)
  )

  for (i in seq_len(n_steps)) {
    # Last steady-state segment of each step (of the data, for a used
    # incomplete last step)
    step_end <- bounds$end[i]
    step_start <- max(1, step_end - bounds$steady_records[i] + 1)

    step_data <- db_MFO[step_start:step_end, , drop = FALSE]

    mean_VO2 <- mean(step_data$VO2, na.rm = TRUE)
    mean_VCO2 <- mean(step_data$VCO2, na.rm = TRUE)
    # HR is optional: a missing column or a step without HR gives NA
    mean_HR <- .mean_or_na(step_data[["HR"]])
    # Load is optional too (SPEC 14)
    mean_Load <- .mean_or_na(step_data[["Load"]])
    mean_RER <- mean(step_data$RER, na.rm = TRUE)

    # Calculate substrate oxidation
    subs <- substrate_oxidation(VO2 = mean_VO2, VCO2 = mean_VCO2,
                                author = author, VO2max = VO2max,
                                energy_equivalents = energy_equivalents)

    db_vars[i, "Load"] <- mean_Load
    db_vars[i, "VO2"] <- mean_VO2
    db_vars[i, "VCO2"] <- mean_VCO2
    db_vars[i, "HR"] <- mean_HR
    db_vars[i, "RER"] <- mean_RER
    db_vars[i, "CHO"] <- subs$CHO
    db_vars[i, "FAT"] <- subs$FAT
    db_vars[i, "Kcal"] <- subs$Kcal
  }

  # Apply RER cutoff if specified
  RER_cutoff <- .resolve_RER_cutoff(RER_cutoff)
  if (!is.null(RER_cutoff)) {
    n_before <- nrow(db_vars)
    db_vars <- db_vars[db_vars$RER <= RER_cutoff, , drop = FALSE]
    n_removed <- n_before - nrow(db_vars)
    if (n_removed > 0) {
      message(sprintf("RER cutoff (%.2f): removed %d step(s) with RER above threshold.",
                      RER_cutoff, n_removed))
    }
  }

  if (author == "Jeukendrup") {
    .warn_jeukendrup_range(db_vars$VO2 / VO2max * 100)
  }

  return(db_vars)
}


# Warn about records left after the last complete step; TRUE if that
# incomplete step is to be used (asked for and >= its steady-state segment)
.incomplete_last_step <- function(available, records, steady_records, step_time,
                                  use_incomplete_step) {
  head <- sprintf("Last step incomplete (%s s of %s s): ",
                  format(available * step_time), format(records * step_time))
  steady_s <- format(steady_records * step_time)
  usable <- available >= steady_records
  if (isTRUE(use_incomplete_step) && usable) {
    warning(head, sprintf("used, averaging its last %s s (use_incomplete_step = TRUE).",
                          steady_s), call. = FALSE)
    return(TRUE)
  }
  if (usable) {
    warning(head, sprintf(paste0("dropped. It holds the %s-s steady-state segment; ",
                                 "set use_incomplete_step = TRUE to use it."),
                          steady_s), call. = FALSE)
  } else {
    warning(head, sprintf(paste0("dropped. It is shorter than steady_state_duration ",
                                 "(%s s) and cannot be used."),
                          steady_s), call. = FALSE)
  }
  FALSE
}


# Mean of the non-missing values; NA (not NaN or a warning) if there are none
.mean_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}


# Named RER cutoffs, Amaro-Gahete et al. (2019, Front Physiol 10: 909,
# doi:10.3389/fphys.2019.00909): 1.00 classic; 0.95 the safe end of the test
# they recommend; 0.93 / 0.90 the mean RER at MFO in sedentary / trained people.
.RER_cutoffs <- c(classic = 1.00, safe = 0.95, sedentary = 0.93, trained = 0.90)

.resolve_RER_cutoff <- function(RER_cutoff) {
  if (is.null(RER_cutoff)) return(NULL)
  if (is.character(RER_cutoff) && length(RER_cutoff) == 1 &&
      RER_cutoff %in% names(.RER_cutoffs)) {
    return(.RER_cutoffs[[RER_cutoff]])
  }
  if (is.numeric(RER_cutoff) && length(RER_cutoff) == 1 && !is.na(RER_cutoff)) {
    return(RER_cutoff)
  }
  stop("RER_cutoff must be NULL, a single number or one of ",
       paste0("\"", names(.RER_cutoffs), "\"", collapse = ", "), ".", call. = FALSE)
}


# Jeukendrup & Wallis (2005, Table 5) equations cover 40-75 %VO2max; outside
# that range the nearest set is extrapolated (SPEC 3, 9.5). Exercise only.
.warn_jeukendrup_range <- function(pct_vo2) {
  out <- which(pct_vo2 < 40 | pct_vo2 > 75)
  if (length(out) > 0) {
    warning(sprintf(paste0(
      "Jeukendrup & Wallis (2005) equations cover 40-75 %%VO2max; %d step(s) ",
      "outside this range (%s %%VO2max) use the nearest set, extrapolated."),
      length(out), paste(sprintf("%.1f", pct_vo2[out]), collapse = ", ")),
      call. = FALSE)
  }
  invisible(out)
}


#' Preprocess breath-by-breath data into fixed intervals
#'
#' Averages breath-by-breath data into fixed time intervals.
#'
#' @param db data frame with breath-by-breath data. Must contain a "Time" column in seconds
#'           and columns for VO2, VCO2, RER, HR.
#' @param target_interval target interval in seconds (default 20).
#'
#' @importFrom stats aggregate
#'
#' @return data frame with data averaged into fixed intervals.
#'
#' @keywords internal
preprocess_bxb <- function(db, target_interval = 20) {

  if (!"Time" %in% colnames(db)) {
    stop("Breath-by-breath data must contain a 'Time' column in seconds.")
  }

  # Create bins
  db$time_bin <- floor(db$Time / target_interval) * target_interval

  # Average within each bin
  vars_to_avg <- intersect(c("VO2", "VCO2", "RER", "HR"), colnames(db))
  result <- aggregate(db[, vars_to_avg, drop = FALSE],
                      by = list(Time = db$time_bin),
                      FUN = mean, na.rm = TRUE)

  return(result)
}


#' Read databases for MFO package
#'
#' @param from select either from "folder" or "files"
#' @param path path to the databases
#' @param db_basal_name name of the database with the basal metabolic rate test
#' @param db_MFO_name name of the database of MFO test
#' @param db_graded_name name of the database of the graded exercise test
#' @param remove_rows An integer (or a vector of integers) representing the position of the rows to delete
#' @param col_name_VO2 name given to the variable VO2 in the databases. Default "VO2"
#' @param col_name_VCO2 name given to the variable VCO2 in the databases. Default "VCO2"
#' @param col_name_RER name given to the variable RER in the databases. Default "RER"
#' @param col_name_HR name given to the variable HR in the databases. Default "HR"
#' @param col_name_Load name given to the external load (W or km/h) in the
#'   databases. Default "Load"; the column is optional.
#'
#'
#' @importFrom dplyr rename select
#' @importFrom readxl read_xlsx
#' @importFrom openxlsx write.xlsx
#'
#' @return {Returns 3 databases:
#' \itemize{
#' \item participant_db_basal: database with basal metabolism.
#' \item participant_db_MFO: database with MFO test.
#' \item participant_db_graded: graded exercise test.
#' }}
#'
#'
#' @keywords internal
read_MFO_databases <- function(from = c("folder", "files"),
                               path,
                               db_basal_name,
                               db_MFO_name,
                               db_graded_name,
                               col_name_VO2 = "VO2",
                               col_name_VCO2 = "VCO2",
                               col_name_RER = "RER",
                               col_name_HR = "HR",
                               remove_rows = NULL,
                               col_name_Load = "Load") {

  from <- match.arg(from)

  if (from == "folder") {
    participant_db_graded <- read_xlsx(paste(path, "/", db_graded_name, ".xlsx", sep = ""))
    participant_db_basal <- read_xlsx(paste(path, "/", db_basal_name, ".xlsx", sep = ""))
    participant_db_MFO <- read_xlsx(paste(path, "/", db_MFO_name, ".xlsx", sep = ""))

  } else if (from == "files") {
    participant_db_graded <- read_xlsx(path = path, sheet = db_graded_name)
    participant_db_basal <- read_xlsx(path = path, sheet = db_basal_name)
    participant_db_MFO <- read_xlsx(path = path, sheet = db_MFO_name)
  }

  # Variables to select from databases
  vars_to_select <- c(col_name_VO2, col_name_VCO2, col_name_RER, col_name_HR,
                      col_name_Load)

  # Standardize column names
  rename_and_clean <- function(db, vars, has_HR = TRUE) {
    db <- db[, intersect(colnames(db), vars), drop = FALSE]

    if (col_name_VO2 != "VO2" && col_name_VO2 %in% colnames(db)) {
      colnames(db)[colnames(db) == col_name_VO2] <- "VO2"
    }
    if (col_name_VCO2 != "VCO2" && col_name_VCO2 %in% colnames(db)) {
      colnames(db)[colnames(db) == col_name_VCO2] <- "VCO2"
    }
    if (col_name_RER != "RER" && col_name_RER %in% colnames(db)) {
      colnames(db)[colnames(db) == col_name_RER] <- "RER"
    }
    if (col_name_HR != "HR" && col_name_HR %in% colnames(db)) {
      colnames(db)[colnames(db) == col_name_HR] <- "HR"
    }
    if (col_name_Load != "Load" && col_name_Load %in% colnames(db)) {
      colnames(db)[colnames(db) == col_name_Load] <- "Load"
    }

    if (!is.null(remove_rows)) {
      db <- db[-c(remove_rows), , drop = FALSE]
    }

    # Convert to numeric; only the gas columns decide which rows are dropped,
    # a missing HR value never removes a row (it would shift the steps)
    db <- as.data.frame(lapply(db, function(x) suppressWarnings(as.numeric(x))))
    gas <- intersect(c("VO2", "VCO2", "RER"), colnames(db))
    db <- db[stats::complete.cases(db[, gas, drop = FALSE]), , drop = FALSE]
    rownames(db) <- NULL
    return(db)
  }

  participant_db_graded <- rename_and_clean(participant_db_graded, vars_to_select)
  participant_db_basal <- rename_and_clean(participant_db_basal, vars_to_select)
  participant_db_MFO <- rename_and_clean(participant_db_MFO, vars_to_select)

  return(list(participant_db_graded = participant_db_graded,
              participant_db_basal = participant_db_basal,
              participant_db_MFO = participant_db_MFO))
}
