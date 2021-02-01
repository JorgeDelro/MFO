## Prueba

library(readxl)
library(tidyverse)
library(minpack.lm)
library(openxlsx)

path = "/Users/jorge/Documents/Paquetes_R/MFO/NUTAF_xlsx"
participants <- list.files(path)

prueba_read <- read_MFO_databases(from = "files",
                                  path = paste(path,"/",participants[2], sep = ""),
                                  db_basal_name = "M.BASAL",
                                  db_MFO_name = "MFO",
                                  db_graded_name = "V02máx.",
                                  col_name_VO2 = "V'O2",
                                  col_name_VCO2 = "V'CO2",
                                  col_name_RER = "RER",
                                  col_name_HR = "HR",
                               remove_rows = 1)


prueba_result_MFO <- MFO(step_time = 20,
                              db_MFO = prueba_read$participant_db_MFO,
                              db_basal = prueba_read$participant_db_basal,
                              db_graded = prueba_read$participant_db_graded,
                              cv_var = "RER",
                              author = "Frayn",
                              VO2max = NULL)

apply(prueba_result_MFO$MFO_db[,-1], 2, abs)

prueba_result_MFO_kinetics <- MFO_kinetics(prueba_result_MFO$MFO_db)


# Examples MFOs
MFOs(from = "files",
            path = "/Users/jorge/Documents/Paquetes_R/MFO/NUTAF_xlsx",
            db_basal_name = "M.BASAL",
            db_MFO_name = "MFO",
            db_graded_name = "V02máx.",
            step_time = 20,
            cv_var = "RER",
            author = "Frayn",
            VO2max = NULL,
            remove_rows = 1,
            col_name_VO2 = "V'O2",
            col_name_VCO2 = "V'CO2",
            col_name_RER = "RER",
            col_name_HR = "HR",
            save_plot = T)





