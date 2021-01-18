
#' Maximal Fat Oxidation calculation of multiple databases
#'
#' @param folders_path path to the folder with the databases
#' @param step_time every few seconds the data was collected
#' @param cv_var variable to estimate coefficient of variation. Can be: VO2, VCO2 or RER.
#' @param author author to estimate MFO. Can be: Frayn or Jeukendrup.
#' @param VO2max VO2max can be passed directly using this argument instead of use db_graded argument. Default set to NULL.
#' @param save_plot to save the plot or not.Default set to True.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' MFOs(folders_path = /Users/username/Documents/R_packages/MFO
#'     step_time = 20,
#'     cv_var = "RER",
#'     author = "Frayn",
#'     VO2max = NULL,
#'     save_plot = T)
#'}
MFOs <- function(folders_path, step_time, cv_var, author, VO2max = NULL, save_plot = T){

  participants <- list.files(folders_path)

  for(i in 1:length(participants)) {
    # Get the participant ID
    participant_db_graded <- read_xlsx(paste(folders_path,"/",participants[i], "/db_graded.xlsx", sep = ""))
    participant_db_basal <- read_xlsx(paste(folders_path,"/",participants[i], "/db_basal.xlsx", sep = ""))
    participant_db_MFO <- read_xlsx(paste(folders_path,"/",participants[i], "/db_MFO.xlsx", sep = ""))

    participant_result_MFO <- MFO(step_time = step_time,
                                  db_MFO = participant_db_MFO,
                                  db_basal = participant_db_basal,
                                  db_graded = participant_db_graded,
                                  cv_var = cv_var,
                                  author = author,
                                  VO2max = VO2max)

    # MFO plot
    if(save_plot == T){
      ggsave(paste(participants[i],".png", sep = ""),
             plot = participant_result_MFO$MFO_plot,
             path = folders_path,
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    participant_result_MFO_kinetics <- MFO_kinetics(participant_result_MFO$MFO_db)

    # Kinetics plot
    if(save_plot == T){
      ggsave(paste(participants[i],"kinetics_.png", sep = ""),
             plot = participant_result_MFO_kinetics$MFO_kinetics_plot,
             path = folders_path,
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    # Data Frame
    if(i == 1){
      dF_result <- tibble(ID = participants[i],
                          MFO_g_min = round(participant_result_MFO$MFO,2),
                          FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX,2),
                          d = round(participant_result_MFO_kinetics$d, 2),
                          t = round(participant_result_MFO_kinetics$t, 2),
                          s = round(participant_result_MFO_kinetics$s, 2),
                          CHO_g_basal = round(participant_result_MFO$x_CHO, 2),
                          FAT_g_basal = round(participant_result_MFO$x_FAT, 2),
                          Kcal_g_basal = round(participant_result_MFO$x_Kcal, 2))
    } else {
      vector_result <- c(ID = participants[i],
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
