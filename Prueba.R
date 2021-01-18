## Prueba

library(readxl)

db_MFO <- read_xlsx("db_MFO.xlsx")
db_graded <- read_xlsx("db_graded.xlsx")
db_basal <- read_xlsx("db_basal.xlsx")

MFO_result <- MFO(step_time = 20, db_MFO = db_MFO, db_graded = db_graded,
    db_basal = db_basal, author = "Frayn", cv_var = "VO2")

MFO_kinetics(MFO_result$MFO_db)
