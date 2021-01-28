# Prepare databases

library(readxl)
library(openxlsx)

write.xlsx(db_basal, file = "prueba_db/db_basal.xlsx")

prepare_databases <- function(folders_path,
                              db_basal_name,
                              db_MFO_name,
                              db_graded_name,
                              remove_row = NULL,
                              output_directory = NULL) {

    if(is.null(output_directory)) {

      output_directory <- dir.create(paste(folders_path,"/MFO_dbs", sep = ""))

    }

  participants <- list.files(folders_path)

  for(i in 1:length(participants)) {
    # Get the participant ID
    participant_db_graded <- read_xlsx(path = paste(folders_path,"/",participants[i], "/db_graded.xlsx", sep = ""),
                                       sheet = db_graded_name)
    participant_db_basal <- read_xlsx(path = paste(folders_path,"/",participants[i], "/db_basal.xlsx", sep = ""),
                                      sheet = db_basal_name)
    participant_db_MFO <- read_xlsx(path = paste(folders_path,"/",participants[i], "/db_MFO.xlsx", sep = ""),
                                    sheet = db_MFO_name)

    output_directory <- dir.create(paste(output_directory,participants[i], sep = ""))

    write.xlsx()

}
}
