
#' Maximal Fat Oxidation calculation of multiple databases
#'
#' Process multiple participants' MFO data in batch, generating plots and summary results.
#' Supports all parameters from the MFO() and MFO_kinetics() functions.
#'
#' @param from select "folder" (basal, MFO and graded databases of the same participant are stored in
#'              different files but in the same folder) or "files" (basal, MFO and graded databases of the
#'              same participant are stored in one file but in different sheets)
#' @param path path to the folder with the databases
#' @param db_basal_name name given to the basal database, e.g. "basal_df"
#' @param db_MFO_name name given to the MFO database, e.g. "MFO_df"
#' @param db_graded_name name given to the graded database, e.g. "VO2max_df"
#' @param step_time how often the data was collected (in seconds).
#' @param cv_var variable to estimate coefficient of variation. Can be: "VO2", "VCO2" or "RER".
#' @param author equation set: "Frayn", "Jeukendrup" or "Peronnet".
#' @param method method to locate MFO: "P3" (default), "P2", "MV" or "MRER". See [MFO()].
#' @param VO2max VO2max can be passed directly (ml/min). Default NULL. If given, it is used for
#'        every participant and the graded databases are ignored.
#' @param vo2max_window averaging window in seconds for VO2max from the graded database
#'        (default 30). See [MFO()].
#' @param include_basal logical; include the resting row as step 0 in the fit
#'        (default `FALSE`). See [MFO()].
#' @param energy_equivalents "classic" (default in 0.2.x) or "source". See [MFO()].
#' @param use_incomplete_step logical; use an incomplete last step that holds the
#'        steady-state segment (default `FALSE`). See [MFO()].
#' @param min_steps minimum number of exercise steps recommended for the fit
#'        (default 6); fewer give a warning. See [MFO()].
#' @param fatmin_threshold `NULL` (default, Fatmin at the second zero of the SIN) or
#'        a proportion of MFO in (0, 1). See [MFO_kinetics()].
#' @param step_duration duration of each exercise step in seconds (default 180).
#' @param steady_state_duration duration of steady-state period within each step in seconds (default 60).
#' @param age age in years (optional), the same for every participant, for the
#'        heart-rate criterion of VO2max. See [MFO()] and [VO2max_from_test()].
#' @param protocol optional [MFO_protocol()], the same for every participant; if
#'        given, `step_duration` and `steady_state_duration` are not used. See [MFO()].
#' @param RER_cutoff RER value above which steps are excluded (default NULL), as a
#'        number or "classic", "safe", "sedentary", "trained". See [MFO()].
#' @param negative_FAT how to handle negative fat oxidation: "truncate", "exclude", or "keep".
#' @param body_mass body mass in kg (optional).
#' @param fat_free_mass fat-free mass in kg (optional).
#' @param HRmax maximum heart rate in bpm (optional).
#' @param remove_rows rows to exclude from analysis.
#' @param col_name_VO2 column name for VO2 in databases. Default "VO2".
#' @param col_name_VCO2 column name for VCO2 in databases. Default "VCO2".
#' @param col_name_RER column name for RER in databases. Default "RER".
#' @param col_name_HR column name for HR in databases. Default "HR".
#' @param col_name_Load column name for the external load (W or km/h) in the
#'        databases. Default "Load"; the column is optional. See [MFO()].
#' @param save_plot save plots to MFO_plots/ directory (default TRUE).
#' @param save_result save results file (default TRUE).
#' @param output_format format for results file: "xlsx", "csv", or "rds". Default "xlsx".
#'
#' @details Every warning raised for a participant is re-issued with the
#' participant ID first (e.g. `"P02: No HR data in db_MFO: ..."`), so the
#' quality warnings of [MFO()] (missing HR, few steps, Fatmax at the edge,
#' incomplete last step) can be traced in a batch. A participant whose data
#' cannot be processed gives a row of `NA` and a warning with the error.
#'
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom openxlsx write.xlsx
#' @importFrom utils write.csv
#'
#' @return {Data frame with one row per participant containing:
#' \itemize{
#' \item ID: Participant identifier.
#' \item MFO_g_min: Maximal fat oxidation (g/min).
#' \item MFO_relative: MFO relative to body mass (mg/kg/min), if body_mass provided.
#' \item MFO_FFM: MFO relative to fat-free mass (mg/kgFFM/min), if fat_free_mass provided.
#' \item VO2max: VO2max used (ml/min).
#' \item VO2max_window: effective averaging window for VO2max (s); NA if VO2max was supplied.
#' \item VO2max_type: "VO2max" or "VO2peak" (see [VO2max_from_test()]); NA if VO2max was supplied.
#' \item FAT_MAX_perVO2max: FatMax as \% VO2max.
#' \item FAT_MAX_HR: Heart rate at FatMax (bpm).
#' \item FAT_MAX_porcHRmax: FatMax as \% HRmax, if HRmax provided.
#' \item FatMax_zone_lower: Lower bound of FatMax zone (\% VO2max).
#' \item FatMax_zone_upper: Upper bound of FatMax zone (\% VO2max).
#' \item FatMax_zone_method: "fitted curve", or "linear interpolation" for "MV".
#' \item FAT_MAX_load, FatMax_zone_lower_load, FatMax_zone_upper_load: Fatmax and
#'   Fatmax zone in load units; load_unit and load_source as in [MFO()].
#' \item crossover_point: Crossover intensity (\% VO2max).
#' \item R_squared: R-squared of the curve fit.
#' \item method: method requested.
#' \item energy_equivalents: "classic" or "source".
#' \item protocol: protocol type (see [MFO_protocol()]).
#' \item mode: exercise mode of the protocol.
#' \item d: SIN dilatation parameter.
#' \item t: SIN translation parameter.
#' \item s: SIN symmetry parameter.
#' \item Fatmin: Fatmin from SIN model (\% VO2max).
#' \item FatMax_SIN: FatMax from SIN model (\% VO2max).
#' \item SIN_R_squared: R-squared of SIN model.
#' \item SIN_convergence: whether the SIN model was fitted.
#' \item CHO_g_basal: Basal CHO oxidation (g/min).
#' \item FAT_g_basal: Basal FAT oxidation (g/min).
#' \item Kcal_g_basal: Basal energy expenditure (kcal/min).
#' }}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # MFOs function
#' results <- MFOs(from = "folder",
#'                 path = "/path/to/participant_data",
#'                 db_basal_name = "basal_df",
#'                 db_MFO_name = "MFO_df",
#'                 db_graded_name = "graded_df",
#'                 step_time = 20,
#'                 cv_var = "RER",
#'                 author = "Frayn",
#'                 method = "P3")
#' }
#'
MFOs <- function(from = c("folder", "files"),
                 path,
                 db_basal_name,
                 db_MFO_name,
                 db_graded_name,
                 step_time,
                 cv_var,
                 author,
                 method = "P3",
                 VO2max = NULL,
                 step_duration = 180,
                 steady_state_duration = 60,
                 RER_cutoff = NULL,
                 negative_FAT = "truncate",
                 body_mass = NULL,
                 fat_free_mass = NULL,
                 HRmax = NULL,
                 remove_rows = NULL,
                 col_name_VO2 = "VO2",
                 col_name_VCO2 = "VCO2",
                 col_name_RER = "RER",
                 col_name_HR = "HR",
                 save_plot = TRUE,
                 save_result = TRUE,
                 output_format = "xlsx",
                 vo2max_window = 30,
                 include_basal = FALSE,
                 fatmin_threshold = NULL,
                 energy_equivalents = c("classic", "source"),
                 use_incomplete_step = FALSE,
                 min_steps = 6,
                 protocol = NULL,
                 age = NULL,
                 col_name_Load = "Load") {

  from <- match.arg(from)
  output_format <- match.arg(output_format, c("xlsx", "csv", "rds"))
  if (missing(energy_equivalents)) .energy_default_message()
  energy_equivalents <- match.arg(energy_equivalents)
  # Resolved once, so a conflict warns once and not per participant
  protocol <- .resolve_protocol(protocol, step_duration, steady_state_duration,
                                durations_given = !missing(step_duration) ||
                                  !missing(steady_state_duration))

  participants <- list.files(path)

  if (length(participants) == 0) {
    stop("No participant folders found in the specified path.")
  }

  # Initialize results list
  results_list <- vector("list", length(participants))

  for (i in seq_along(participants)) {

    ID_participant <- participants[i]
    message(paste("Processing:", ID_participant))

    tryCatch(withCallingHandlers({
      # Read databases
      participant_dbs <- read_MFO_databases(
        from = from,
        path = paste(path, "/", participants[i], sep = ""),
        db_basal_name = db_basal_name,
        db_MFO_name = db_MFO_name,
        db_graded_name = db_graded_name,
        remove_rows = remove_rows,
        col_name_VO2 = col_name_VO2,
        col_name_VCO2 = col_name_VCO2,
        col_name_RER = col_name_RER,
        col_name_HR = col_name_HR,
        col_name_Load = col_name_Load
      )

      # Calculate MFO
      participant_result_MFO <- MFO(
        step_time = step_time,
        db_MFO = participant_dbs$participant_db_MFO,
        db_basal = participant_dbs$participant_db_basal,
        # A supplied VO2max wins; skip db_graded to avoid a warning per participant
        db_graded = if (is.null(VO2max)) participant_dbs$participant_db_graded else NULL,
        cv_var = cv_var,
        author = author,
        VO2max = VO2max,
        method = method,
        protocol = protocol,
        RER_cutoff = RER_cutoff,
        negative_FAT = negative_FAT,
        body_mass = body_mass,
        fat_free_mass = fat_free_mass,
        HRmax = HRmax,
        vo2max_window = vo2max_window,
        include_basal = include_basal,
        energy_equivalents = energy_equivalents,
        use_incomplete_step = use_incomplete_step,
        min_steps = min_steps,
        age = age
      )

      # Save MFO plot
      if (save_plot) {
        if (i == 1) {
          plot_dir <- paste(getwd(), "/MFO_plots", sep = "")
          if (!dir.exists(plot_dir)) dir.create(plot_dir)
        }
        ggsave(paste(ID_participant, "_MFO.png", sep = ""),
               plot = participant_result_MFO$MFO_plot,
               path = paste(getwd(), "/MFO_plots", sep = ""),
               height = 5, width = 7, units = "in", dpi = 600)
      }

      # Calculate kinetics
      participant_result_kinetics <- MFO_kinetics(participant_result_MFO$MFO_db,
                                                  fatmin_threshold = fatmin_threshold)

      # Save kinetics plot
      if (save_plot) {
        ggsave(paste(ID_participant, "_kinetics.png", sep = ""),
               plot = participant_result_kinetics$MFO_kinetics_plot,
               path = paste(getwd(), "/MFO_plots", sep = ""),
               height = 5, width = 7, units = "in", dpi = 600)
      }

      # Collect results
      results_list[[i]] <- data.frame(
        ID = ID_participant,
        MFO_g_min = round(participant_result_MFO$MFO, 4),
        MFO_relative = round(ifelse(is.na(participant_result_MFO$MFO_relative), NA,
                                    participant_result_MFO$MFO_relative), 2),
        MFO_FFM = round(ifelse(is.na(participant_result_MFO$MFO_FFM), NA,
                               participant_result_MFO$MFO_FFM), 2),
        VO2max = round(participant_result_MFO$VO2max, 1),
        VO2max_window = participant_result_MFO$VO2max_window,
        VO2max_type = participant_result_MFO$VO2max_type,
        FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX, 2),
        FAT_MAX_HR = round(ifelse(is.na(participant_result_MFO$FAT_MAX_HR), NA,
                                  participant_result_MFO$FAT_MAX_HR), 1),
        FAT_MAX_porcHRmax = round(ifelse(is.na(participant_result_MFO$FAT_MAX_porcHRmax), NA,
                                          participant_result_MFO$FAT_MAX_porcHRmax), 2),
        FatMax_zone_lower = round(ifelse(is.na(participant_result_MFO$FatMax_zone_lower), NA,
                                          participant_result_MFO$FatMax_zone_lower), 2),
        FatMax_zone_upper = round(ifelse(is.na(participant_result_MFO$FatMax_zone_upper), NA,
                                          participant_result_MFO$FatMax_zone_upper), 2),
        FatMax_zone_method = participant_result_MFO$FatMax_zone_method,
        FAT_MAX_load = round(participant_result_MFO$FAT_MAX_load, 1),
        FatMax_zone_lower_load = round(participant_result_MFO$FatMax_zone_lower_load, 1),
        FatMax_zone_upper_load = round(participant_result_MFO$FatMax_zone_upper_load, 1),
        load_unit = participant_result_MFO$load_unit,
        load_source = participant_result_MFO$load_source,
        crossover_point = round(ifelse(is.na(participant_result_MFO$crossover_point), NA,
                                       participant_result_MFO$crossover_point), 2),
        R_squared = round(participant_result_MFO$R_squared, 4),
        method = participant_result_MFO$method,
        energy_equivalents = participant_result_MFO$energy_equivalents,
        protocol = participant_result_MFO$protocol$type,
        mode = participant_result_MFO$mode,
        d = round(participant_result_kinetics$d, 3),
        t = round(participant_result_kinetics$t, 3),
        s = round(participant_result_kinetics$s, 3),
        Fatmin = round(ifelse(is.na(participant_result_kinetics$Fatmin), NA,
                              participant_result_kinetics$Fatmin), 2),
        FatMax_SIN = round(ifelse(is.na(participant_result_kinetics$FatMax_SIN), NA,
                                   participant_result_kinetics$FatMax_SIN), 2),
        SIN_R_squared = round(ifelse(is.na(participant_result_kinetics$R_squared), NA,
                                     participant_result_kinetics$R_squared), 4),
        SIN_convergence = participant_result_kinetics$convergence,
        CHO_g_basal = round(participant_result_MFO$x_CHO, 4),
        FAT_g_basal = round(participant_result_MFO$x_FAT, 4),
        Kcal_g_basal = round(participant_result_MFO$x_Kcal, 4),
        stringsAsFactors = FALSE
      )

    }, warning = function(w) {
      # Name the participant in every warning, so a batch shows whose result
      # needs checking (e.g. missing HR, few steps; SPEC 9.5, 14)
      warning(paste0(ID_participant, ": ", conditionMessage(w)), call. = FALSE)
      invokeRestart("muffleWarning")
    }), error = function(e) {
      warning(paste("Error processing", ID_participant, ":", e$message))
      results_list[[i]] <<- data.frame(
        ID = ID_participant,
        MFO_g_min = NA, MFO_relative = NA, MFO_FFM = NA,
        VO2max = NA, VO2max_window = NA, VO2max_type = NA,
        FAT_MAX_perVO2max = NA, FAT_MAX_HR = NA, FAT_MAX_porcHRmax = NA,
        FatMax_zone_lower = NA, FatMax_zone_upper = NA, FatMax_zone_method = NA,
        FAT_MAX_load = NA, FatMax_zone_lower_load = NA, FatMax_zone_upper_load = NA,
        load_unit = NA, load_source = NA,
        crossover_point = NA, R_squared = NA, method = method,
        energy_equivalents = energy_equivalents,
        protocol = protocol$type, mode = protocol$mode,
        d = NA, t = NA, s = NA, Fatmin = NA, FatMax_SIN = NA,
        SIN_R_squared = NA, SIN_convergence = FALSE,
        CHO_g_basal = NA, FAT_g_basal = NA, Kcal_g_basal = NA,
        stringsAsFactors = FALSE
      )
    })

  } # end loop

  # Combine results
  dF_result <- do.call(rbind, results_list)

  # Save results
  if (save_result) {
    result_file <- paste("MFO_result.", output_format, sep = "")
    if (output_format == "xlsx") {
      write.xlsx(dF_result, result_file)
    } else if (output_format == "csv") {
      write.csv(dF_result, result_file, row.names = FALSE)
    } else if (output_format == "rds") {
      saveRDS(dF_result, result_file)
    }
    message(paste("Results saved to:", result_file))
  }

  return(dF_result)
}
