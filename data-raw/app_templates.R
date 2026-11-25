# Templates of the app (#24; Jorge, 2026-11-25), downloadable from the tab
# "Prepare your data" and from Reliability. Run from the package root:
#   Rscript data-raw/app_templates.R
devtools::load_all(quiet = TRUE)
out <- "inst/app/templates"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# 1. Individual test: one xlsx with a sheet per phase, the columns of the
# data contract (Time in s, VO2 and VCO2 in mL/min, RER, HR, Load in W),
# from the example data of the package
sheet <- function(db, load = FALSE) {
  db <- as.data.frame(db)
  tm <- as.numeric(db$Time)
  cols <- c("VO2", "VCO2", "RER", "HR", if (load) "Load")
  cbind(Time = tm - tm[1] + (tm[2] - tm[1]), db[cols])
}
openxlsx::write.xlsx(list(Rest = sheet(basal_df), MFO = sheet(MFO_df, TRUE),
                          Graded = sheet(VO2max_df, TRUE)),
                     file.path(out, "MFO_test_template.xlsx"), overwrite = TRUE)

# 2. Results table for Reliability: 2 participants x 2 sessions of simulated
# tests, written with write_MFO_sessions() and analysed with MFOs(). The
# required columns are id (participant), session, MFO (g/min) and Fatmax
# (% VO2max); the other columns of MFOs() are optional
sim <- simulate_MFO_sessions(n_participants = 2, n_sessions = 2,
                             population = list(VO2max = c(3000, 500)), seed = 1)
dir <- file.path(tempdir(), "mfo_template_sessions")
unlink(dir, recursive = TRUE)
write_MFO_sessions(sim, dir)
res <- suppressWarnings(suppressMessages(
  MFOs(from = "folder", path = dir, db_basal_name = "basal_df", db_MFO_name = "MFO_df",
       db_graded_name = "VO2max_df", step_time = sim$settings$step_time, cv_var = "RER",
       author = "Frayn", energy_equivalents = "classic")))
idx <- utils::read.csv(file.path(dir, "sessions.csv"), stringsAsFactors = FALSE)
m <- match(res$ID, idx$id)
optional <- c("FatMax_zone_lower", "FatMax_zone_upper", "FAT_MAX_porcHRmax")
optional <- optional[vapply(optional, function(k) k %in% names(res) && !all(is.na(res[[k]])),
                            logical(1))]
tab <- data.frame(id = idx$participant[m], session = idx$session[m],
                  MFO = round(res$MFO_g_min, 3), Fatmax = round(res$FAT_MAX_perVO2max, 1),
                  lapply(res[optional], function(x) round(x, 3)), check.names = FALSE)
utils::write.csv(tab, file.path(out, "MFO_results_template.csv"), row.names = FALSE)
print(tab)
