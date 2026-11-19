# Builds data/reference_MFO.rda from Table 1 of Amaro-Gahete, Sanchez-Delgado
# and Ruiz (2018), Front Physiol 9:1460, doi:10.3389/fphys.2018.01460.
# The table is parsed from data-raw/Amaro_Gahete_2018.md (verified by Jorge
# against the published text, 19 Nov 2026), so no value is typed by hand.
# SPEC 15.2 / 19. Run from the package root: source("data-raw/reference_MFO.R")

lines <- readLines("data-raw/Amaro_Gahete_2018.md", encoding = "UTF-8")
rows <- grep("^\\| (All|By ) ?", lines, value = TRUE)
rows <- grep("^\\| Grouping", rows, value = TRUE, invert = TRUE)
cells <- lapply(strsplit(rows, "|", fixed = TRUE), function(x) trimws(x[-1]))
tab <- as.data.frame(do.call(rbind, cells), stringsAsFactors = FALSE)
stopifnot(ncol(tab) == 13, nrow(tab) == 12)

mean_sd <- function(x) {
  p <- as.numeric(strsplit(x, "±", fixed = TRUE)[[1]])
  stopifnot(length(p) == 2)
  p
}
mfo <- t(vapply(tab[[4]], mean_sd, numeric(2)))
fmx <- t(vapply(tab[[9]], mean_sd, numeric(2)))

population <- tab[[2]]
sex <- ifelse(grepl("women", population), "female",
              ifelse(grepl("men", population), "male", NA_character_))
age_group <- ifelse(grepl("young", population), "young",
                    ifelse(grepl("middle-aged", population), "middle-aged", NA_character_))
weight_status <- ifelse(grepl("^Normal-weight", population), "normal",
                        ifelse(grepl("^Overweight", population), "overweight",
                               ifelse(grepl("^Obese", population), "obese", NA_character_)))

reference_MFO <- data.frame(
  cohort = "Amaro-Gahete 2018",
  grouping = tab[[1]],
  population = population,
  sex = sex,
  age_group = age_group,
  weight_status = weight_status,
  n = as.integer(tab[[3]]),
  MFO_mean = unname(mfo[, 1]),
  MFO_sd = unname(mfo[, 2]),
  MFO_p20 = as.numeric(tab[[5]]),
  MFO_p40 = as.numeric(tab[[6]]),
  MFO_p60 = as.numeric(tab[[7]]),
  MFO_p80 = as.numeric(tab[[8]]),
  Fatmax_mean = unname(fmx[, 1]),
  Fatmax_sd = unname(fmx[, 2]),
  Fatmax_p20 = as.numeric(tab[[10]]),
  Fatmax_p40 = as.numeric(tab[[11]]),
  Fatmax_p60 = as.numeric(tab[[12]]),
  Fatmax_p80 = as.numeric(tab[[13]]),
  mode = "walking",
  protocol = paste("Treadmill, 3 min at 3.5 km/h (0 % gradient), +1 km/h every",
                   "3 min up to the maximal walking speed, then +2 % gradient",
                   "every 3 min until RER >= 1.0; last 60 s of each step"),
  equation = "Frayn (no protein)",
  method = "P3 through the origin (0;0) (Croci et al. 2014)",
  fasting = "5-6 h (not overnight)",
  doi = "10.3389/fphys.2018.01460",
  stringsAsFactors = FALSE
)
rownames(reference_MFO) <- NULL

save(reference_MFO, file = "data/reference_MFO.rda", compress = "bzip2", version = 2)
