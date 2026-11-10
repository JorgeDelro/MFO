
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
#'
#' @importFrom dplyr pull mutate
#' @importFrom tibble add_column
#'
met_basal <- function(step_time, db, cv_var, window_duration = 300,
                      author = "Frayn") {

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
                              author = author, phase = "rest")
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
#'
#' @details
#' Rule: a coefficient printed in the source is used as printed; a derived
#' coefficient is used only where the source prints no equation (SPEC 19).
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
#' @return Named list with CHO (g/min), FAT (g/min), Kcal (kcal/min)
#'
substrate_oxidation <- function(VO2, VCO2, author, VO2max = NULL,
                                phase = c("exercise", "rest")) {

  phase <- match.arg(phase)

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
    # Peronnet & Massicotte (1991)
    CHO <- 4.585 * vco2 - 3.2255 * vo2
    FAT <- 1.6946 * vo2 - 1.7012 * vco2

  } else {
    stop("author must be 'Frayn', 'Jeukendrup', or 'Peronnet'")
  }

  Kcal <- CHO * 4 + FAT * 9

  return(list(CHO = CHO, FAT = FAT, Kcal = Kcal))
}


#' Calculate MFO using the MRER method (Perez-Martin et al.)
#'
#' @param VO2 vector of oxygen consumption values (ml/min)
#' @param RER vector of respiratory exchange ratio values
#'
#' @return fat oxidation rate in g/min
#'
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
#' @param RER_cutoff RER value above which data is excluded (default NULL = no cutoff).
#'
#' @details With `author = "Jeukendrup"`, a single warning lists the steps (after
#' `RER_cutoff`) whose intensity is outside 40-75 \% VO2max, the range of the
#' Jeukendrup & Wallis (2005, Table 5) equations (SPEC 3, 9.5).
#'
#' @importFrom dplyr pull mutate
#'
#' @return data frame with columns: VO2, VCO2, HR, RER, CHO, FAT, Kcal (one row per step)
#'
calculate_vars <- function(step_time,
                           db_MFO,
                           VO2max,
                           author,
                           step_duration = 180,
                           steady_state_duration = 60,
                           RER_cutoff = NULL) {

  records_per_step <- as.integer(step_duration / step_time)
  records_steady <- as.integer(steady_state_duration / step_time)
  n_steps <- as.integer(nrow(db_MFO) / records_per_step)

  if (n_steps < 1) {
    stop("Not enough data for at least one complete step.")
  }

  db_vars <- data.frame(
    VO2 = numeric(n_steps),
    VCO2 = numeric(n_steps),
    HR = numeric(n_steps),
    RER = numeric(n_steps),
    CHO = numeric(n_steps),
    FAT = numeric(n_steps),
    Kcal = numeric(n_steps)
  )

  for (i in seq_len(n_steps)) {
    # Last steady_state_duration seconds of each step
    step_end <- i * records_per_step
    step_start <- step_end - records_steady + 1

    # Ensure bounds are within data
    step_start <- max(1, step_start)
    step_end <- min(nrow(db_MFO), step_end)

    step_data <- db_MFO[step_start:step_end, , drop = FALSE]

    mean_VO2 <- mean(step_data$VO2, na.rm = TRUE)
    mean_VCO2 <- mean(step_data$VCO2, na.rm = TRUE)
    mean_HR <- mean(step_data$HR, na.rm = TRUE)
    mean_RER <- mean(step_data$RER, na.rm = TRUE)

    # Calculate substrate oxidation
    subs <- substrate_oxidation(VO2 = mean_VO2, VCO2 = mean_VCO2,
                                author = author, VO2max = VO2max)

    db_vars[i, "VO2"] <- mean_VO2
    db_vars[i, "VCO2"] <- mean_VCO2
    db_vars[i, "HR"] <- mean_HR
    db_vars[i, "RER"] <- mean_RER
    db_vars[i, "CHO"] <- subs$CHO
    db_vars[i, "FAT"] <- subs$FAT
    db_vars[i, "Kcal"] <- subs$Kcal
  }

  # Apply RER cutoff if specified
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
read_MFO_databases <- function(from = c("folder", "files"),
                               path,
                               db_basal_name,
                               db_MFO_name,
                               db_graded_name,
                               col_name_VO2 = "VO2",
                               col_name_VCO2 = "VCO2",
                               col_name_RER = "RER",
                               col_name_HR = "HR",
                               remove_rows = NULL) {

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
  vars_to_select <- c(col_name_VO2, col_name_VCO2, col_name_RER, col_name_HR)

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

    if (!is.null(remove_rows)) {
      db <- db[-c(remove_rows), , drop = FALSE]
    }

    # Convert to numeric
    db <- as.data.frame(apply(db, 2, as.numeric))
    db <- na.omit(db)
    return(db)
  }

  participant_db_graded <- rename_and_clean(participant_db_graded, vars_to_select)
  participant_db_basal <- rename_and_clean(participant_db_basal, vars_to_select)
  participant_db_MFO <- rename_and_clean(participant_db_MFO, vars_to_select)

  return(list(participant_db_graded = participant_db_graded,
              participant_db_basal = participant_db_basal,
              participant_db_MFO = participant_db_MFO))
}
