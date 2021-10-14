#' Maximal Fat Oxidation calculation of multiple databases
#'
#' @param folders_path path to the folder with the databases
#' @param step_time how often the data was collected (in seconds).
#' @param cv_var variable to estimate coefficient of variation. Can be: VO2, VCO2 or RER.
#' @param author author to estimate MFO. Can be: Frayn or Jeukendrup.
#' @param VO2max VO2max can be passed directly using this argument instead of use db_graded argument. Default set to NULL.
#' @param save_plot to save the plot or not.Default set to True.
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom openxlsx write.xlsx
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Path to the MFO package sample data FOLDER
#' path <- system.file("extdata", package = "MFO")
#'
#' # Calculate MFO and Fatmax
#' MFOs(folders_path = path
#'     step_time = 20,
#'     cv_var = "RER",
#'     author = "Frayn",
#'     VO2max = NULL,
#'     save_plot = T)
#'}
MFOs <- function(from = c("folder", "files"),
                 path,
                 db_basal_name,
                 db_MFO_name,
                 db_graded_name,
                 step_time,
                 cv_var,
                 author,
                 VO2max = NULL,
                 remove_rows = NULL,
                 col_name_VO2 = "VO2",
                 col_name_VCO2 = "VCO2",
                 col_name_RER = "RER",
                 col_name_HR = "HR",
                 save_plot = T){

  participants <- list.files(path)

  #print(participants)

  for(i in 1:length(participants)) {

    # Get the participant ID
    ID_participant <- str_subset(participants[i], ".xlsx")

    print(paste("Working in ", participants[i], sep = ""))

    participant_dbs <- read_MFO_databases(from = from,
                                          path = paste(path,"/",participants[i], sep = ""),
                                          db_basal_name = db_basal_name,
                                          db_MFO_name = db_MFO_name,
                                          db_graded_name = db_graded_name,
                                          remove_rows = remove_rows,
                                          col_name_VO2 = col_name_VO2,
                                          col_name_VCO2 = col_name_VCO2,
                                          col_name_RER = col_name_RER,
                                          col_name_HR = col_name_HR)

    participant_result_MFO <- MFO(step_time = step_time,
                                  db_MFO = participant_dbs$participant_db_MFO,
                                  db_basal = participant_dbs$participant_db_basal,
                                  db_graded = participant_dbs$participant_db_graded,
                                  cv_var = cv_var,
                                  author = author,
                                  VO2max = VO2max)

    # MFO plot
    if(save_plot == T){

      # Create a folder to store plots
      dir.create(paste(path,"/MFO_plots", sep = ""))

      ggsave(paste(ID_participant,".png", sep = ""),
             plot = participant_result_MFO$MFO_plot,
             path = paste(path,"/MFO_plots", sep = ""),
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    participant_result_MFO_kinetics <- MFO_kinetics(participant_result_MFO$MFO_db)

    # Kinetics plot
    if(save_plot == T){
      ggsave(paste(ID_participant,"kinetics_.png", sep = ""),
             plot = participant_result_MFO_kinetics$MFO_kinetics_plot,
             path = paste(path,"/MFO_plots", sep = ""),
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    # Data Frame
    if(i == 1){
      dF_result <- tibble(ID = ID_participant,
                          MFO_g_min = round(participant_result_MFO$MFO,2),
                          FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX,2),
                          d = round(participant_result_MFO_kinetics$d, 2),
                          t = round(participant_result_MFO_kinetics$t, 2),
                          s = round(participant_result_MFO_kinetics$s, 2),
                          CHO_g_basal = round(participant_result_MFO$x_CHO, 2),
                          FAT_g_basal = round(participant_result_MFO$x_FAT, 2),
                          Kcal_g_basal = round(participant_result_MFO$x_Kcal, 2))
    } else {
      vector_result <- c(ID = ID_participant,
                         MFO_g_min = round(participant_result_MFO$MFO,2),
                         FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX,2),
                         d = round(participant_result_MFO_kinetics$d, 2),
                         t = round(participant_result_MFO_kinetics$t, 2),
                         s = round(participant_result_MFO_kinetics$s, 2),
                         CHO_g_basal = round(participant_result_MFO$x_CHO, 2),
                         FAT_g_basal = round(participant_result_MFO$x_FAT, 2),
                         Kcal_g_basal = round(participant_result_MFO$x_Kcal, 2))

      dF_result <- rbind(dF_result, vector_result)
    }


  } # end loop


  write.xlsx(dF_result, "MFO_result.xlsx")

}
