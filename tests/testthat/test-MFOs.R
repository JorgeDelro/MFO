
# MFOs() batch tests (SPEC 9.5, 14). Two synthetic participants built from the
# example data: P01 = example data as shipped; P02 = VO2/VCO2 scaled by 1.1
# (same RER) and no HR column, so its results differ and the missing-HR
# warning must name it. Expected values: MFO() called participant by
# participant on the same tables, rounded as the summary rounds them
# (MFO and basal 4 decimals, %VO2max 2, VO2max and HR 1); then exact.

.participants <- function() {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  p1 <- list(basal = data.frame(basal_df), MFO = data.frame(MFO_df),
             graded = data.frame(VO2max_df))
  scale_gas <- function(db) {
    db$VO2 <- db$VO2 * 1.1
    db$VCO2 <- db$VCO2 * 1.1
    db
  }
  p2 <- lapply(p1, scale_gas)
  p2$MFO$HR <- NULL
  list(P01 = p1, P02 = p2)
}

# Tables as read_MFO_databases() returns them (selected, numeric columns)
.as_read <- function(db) db[, intersect(c("VO2", "VCO2", "RER", "HR"), names(db))]

.write_folder <- function(root, parts) {
  for (id in names(parts)) {
    dir.create(file.path(root, id))
    openxlsx::write.xlsx(parts[[id]]$basal, file.path(root, id, "basal_df.xlsx"))
    openxlsx::write.xlsx(parts[[id]]$MFO, file.path(root, id, "MFO_df.xlsx"))
    openxlsx::write.xlsx(parts[[id]]$graded, file.path(root, id, "VO2max_df.xlsx"))
  }
}

.mfos <- function(from, path, ...) {
  MFOs(from = from, path = path, db_basal_name = "basal_df",
       db_MFO_name = "MFO_df", db_graded_name = "VO2max_df", step_time = 20,
       cv_var = "RER", author = "Frayn", energy_equivalents = "classic",
       save_plot = FALSE, save_result = FALSE, ...)
}

.expect_matches_MFO <- function(row, p, VO2max = NULL, ...) {
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = .as_read(p$MFO), db_basal = .as_read(p$basal),
        db_graded = if (is.null(VO2max)) .as_read(p$graded) else NULL,
        VO2max = VO2max, cv_var = "RER", author = "Frayn",
        energy_equivalents = "classic", ...)))
  expect_equal(row$MFO_g_min, round(r$MFO, 4))
  expect_equal(row$FAT_MAX_perVO2max, round(r$FAT_MAX, 2))
  expect_equal(row$FatMax_zone_lower, round(r$FatMax_zone_lower, 2))
  expect_equal(row$FatMax_zone_upper, round(r$FatMax_zone_upper, 2))
  expect_equal(row$VO2max, round(r$VO2max, 1))
  expect_equal(row$VO2max_window, r$VO2max_window)
  expect_equal(row$FatMax_zone_method, r$FatMax_zone_method)
  expect_equal(row$energy_equivalents, r$energy_equivalents)
  expect_equal(row$CHO_g_basal, round(r$x_CHO, 4))
  expect_equal(row$FAT_MAX_HR, round(r$FAT_MAX_HR, 1))
  invisible(r)
}

new_cols <- c("VO2max", "VO2max_window", "FatMax_zone_method", "energy_equivalents")

test_that("MFOs(from = 'folder') gives one row per participant matching MFO()", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts)

  w <- capture_warnings(res <- suppressMessages(.mfos("folder", root)))
  expect_equal(nrow(res), 2)
  expect_equal(res$ID, c("P01", "P02"))
  expect_true(all(new_cols %in% names(res)))
  # 20-s records: the 30-s VO2max window is rounded up to 40 s (SPEC 9.5)
  expect_equal(res$VO2max_window, c(40, 40))
  expect_equal(res$FatMax_zone_method, c("fitted curve", "fitted curve"))
  expect_equal(res$energy_equivalents, c("classic", "classic"))
  .expect_matches_MFO(res[1, ], parts$P01)
  .expect_matches_MFO(res[2, ], parts$P02)
  # the participants really differ, and P02 has no HR
  expect_false(isTRUE(all.equal(res$MFO_g_min[1], res$MFO_g_min[2])))
  expect_false(is.na(res$FAT_MAX_HR[1]))
  expect_true(is.na(res$FAT_MAX_HR[2]))
  # warnings carry the participant ID: the missing-HR one is P02's only
  expect_match(w, "^P02: No HR data in db_MFO", all = FALSE)
  expect_false(any(grepl("^P01: No HR", w)))
})

test_that("MFOs(from = 'files') reads one xlsx with 3 sheets per participant", {
  root <- withr::local_tempdir()
  parts <- .participants()
  for (id in names(parts)) {
    openxlsx::write.xlsx(list(basal_df = parts[[id]]$basal,
                              MFO_df = parts[[id]]$MFO,
                              VO2max_df = parts[[id]]$graded),
                         file.path(root, paste0(id, ".xlsx")))
  }
  expect_equal(readxl::excel_sheets(file.path(root, "P01.xlsx")),
               c("basal_df", "MFO_df", "VO2max_df"))

  # "files": path is the folder; each participant is one workbook
  res <- suppressWarnings(suppressMessages(.mfos("files", root)))
  expect_equal(nrow(res), 2)
  expect_equal(res$ID, c("P01.xlsx", "P02.xlsx"))
  expect_true(all(new_cols %in% names(res)))
  .expect_matches_MFO(res[1, ], parts$P01)
  .expect_matches_MFO(res[2, ], parts$P02)
})

test_that("MFOs() passes method, VO2max and the quality arguments to MFO()", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts)

  # MV: zone by linear interpolation; supplied VO2max, so no window
  res <- suppressWarnings(suppressMessages(
    .mfos("folder", root, method = "MV", VO2max = 3300)))
  expect_equal(res$FatMax_zone_method, rep("linear interpolation", 2))
  expect_equal(res$VO2max, c(3300, 3300))
  expect_true(all(is.na(res$VO2max_window)))
  .expect_matches_MFO(res[1, ], parts$P01, VO2max = 3300, method = "MV")
  .expect_matches_MFO(res[2, ], parts$P02, VO2max = 3300, method = "MV")

  # min_steps and RER_cutoff reach MFO(): the warning names each participant
  w <- capture_warnings(suppressMessages(
    .mfos("folder", root, RER_cutoff = "sedentary")))
  expect_match(w, "^P01: The curve is fitted with 5 exercise step", all = FALSE)
  w <- capture_warnings(suppressMessages(
    .mfos("folder", root, RER_cutoff = "sedentary", min_steps = 5)))
  expect_false(any(grepl("min_steps", w)))
})

test_that("a participant that cannot be processed gives an NA row and a warning", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts["P01"])
  dir.create(file.path(root, "P03")) # empty folder: no files to read

  w <- capture_warnings(res <- suppressMessages(.mfos("folder", root)))
  expect_equal(nrow(res), 2)
  expect_true(is.na(res$MFO_g_min[2]))
  expect_equal(res$energy_equivalents[2], "classic")
  expect_match(w, "Error processing P03", all = FALSE)
  expect_error(.mfos("folder", withr::local_tempdir()), "No participant folders")
})

test_that("MFOs() saves plots and results in xlsx, csv and rds", {
  root <- withr::local_tempdir()
  .write_folder(root, .participants()["P01"])
  out <- withr::local_tempdir()
  withr::local_dir(out)

  for (fmt in c("xlsx", "csv", "rds")) {
    res <- suppressWarnings(suppressMessages(
      MFOs(from = "folder", path = root, db_basal_name = "basal_df",
           db_MFO_name = "MFO_df", db_graded_name = "VO2max_df", step_time = 20,
           cv_var = "RER", author = "Frayn", energy_equivalents = "classic",
           save_plot = fmt == "xlsx", save_result = TRUE, output_format = fmt)))
    expect_true(file.exists(file.path(out, paste0("MFO_result.", fmt))))
  }
  expect_true(file.exists(file.path(out, "MFO_plots", "P01_MFO.png")))
  expect_true(file.exists(file.path(out, "MFO_plots", "P01_kinetics.png")))
  expect_equal(readRDS(file.path(out, "MFO_result.rds")), res)
})
