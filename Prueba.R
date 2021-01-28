## Prueba

library(readxl)
library(tidyverse)

path = "/Users/jorge/Documents/Paquetes_R/MFO/prueba_db"
participants <- list.files(path)

prueba_read <- read_MFO_databases(from = "files",
                                  path = paste(path,"/",participants[1], sep = ""),
                                  db_basal_name = "M.BASAL",
                                  db_MFO_name = "MFO",
                                  db_graded_name = "V02máx.",
                                  col_name_VO2 = "V'O2",
                                  col_name_VCO2 = "V'CO2",
                                  col_name_RER = "RER",
                               remove_rows = 1)

r_rows <- c(1,3,4)

prueba_result_MFO <- MFO(step_time = 20,
                              db_MFO = prueba_read$participant_db_MFO,
                              db_basal = prueba_read$participant_db_basal,
                              db_graded = prueba_read$participant_db_graded,
                              cv_var = "RER",
                              author = "Frayn",
                              VO2max = NULL)



MFOs(from = "files",
            path = "/Users/jorge/Documents/Paquetes_R/MFO/prueba_db",
            db_basal_name = "M.BASAL",
            db_MFO_name = "MFO",
            db_graded_name = "V02máx.",
            step_time = 20,
            cv_var = "RER",
            author = "Frayn",
            VO2max = NULL,
            remove_rows = 1,
            col_name_VO2 = "VO2",
            col_name_VCO2 = "VCO2",
            col_name_RER = "RER",
            save_plot = T)
