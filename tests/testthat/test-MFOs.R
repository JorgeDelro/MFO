
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
  expect_equal(res$method_used, c("P3", "P3"))
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

  # "files": path is the folder; each participant is one workbook. Other
  # files (here a note and a csv) are not participants
  writeLines("notes", file.path(root, "notes.txt"))
  write.csv(data.frame(x = 1), file.path(root, "sessions.csv"))
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

test_that("MFOs() passes the protocol to MFO() and reports it", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts)
  p <- MFO_protocol("step_variable", step_duration = c(360, 180), mode = "running")
  res <- suppressWarnings(suppressMessages(.mfos("folder", root, protocol = p)))
  expect_equal(res$protocol, c("step_variable", "step_variable"))
  expect_equal(res$mode, c("running", "running"))
  .expect_matches_MFO(res[1, ], parts$P01, protocol = p)
  # a conflicting step_duration warns once for the batch, not per participant
  w <- capture_warnings(suppressMessages(.mfos("folder", root, protocol = p,
                                               step_duration = 120)))
  expect_equal(sum(grepl("using protocol", w)), 1)
})

test_that("MFOs() reads a renamed Load column and reports load and VO2max type", {
  root <- withr::local_tempdir()
  parts <- .participants()
  # the analyser calls the load "Power"
  for (id in names(parts)) {
    names(parts[[id]]$MFO)[names(parts[[id]]$MFO) == "Load"] <- "Power"
  }
  .write_folder(root, parts)
  res <- suppressWarnings(suppressMessages(
    .mfos("folder", root, col_name_Load = "Power", age = 30)))
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20,
        db_MFO = cbind(.as_read(.participants()$P01$MFO), Load = .participants()$P01$MFO$Load),
        db_basal = .as_read(parts$P01$basal), db_graded = .as_read(parts$P01$graded),
        cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30)))
  expect_equal(res$load_source, c("file", "file"))
  expect_equal(res$load_unit, c("W", "W"))
  expect_equal(res$FAT_MAX_load[1], round(r$FAT_MAX_load, 1))
  expect_equal(res$FatMax_zone_lower_load[1], round(r$FatMax_zone_lower_load, 1))
  expect_equal(res$VO2max_type[1], r$VO2max_type)
  # without the column name the load is lost: no load outputs
  res0 <- suppressWarnings(suppressMessages(.mfos("folder", root)))
  expect_equal(res0$load_source, c("none", "none"))
  expect_true(all(is.na(res0$FAT_MAX_load)))
})

test_that("MFOs() passes ci = 'bootstrap' and reports the limits", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts)
  res <- suppressWarnings(suppressMessages(.mfos("folder", root, ci = "bootstrap",
                                                 B = 100, seed = 2)))
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = .as_read(parts$P01$MFO), db_basal = .as_read(parts$P01$basal),
        db_graded = .as_read(parts$P01$graded), cv_var = "RER", author = "Frayn",
        energy_equivalents = "classic", ci = "bootstrap", B = 100, seed = 2)))
  expect_equal(res$FAT_MAX_lower[1], round(r$ci["FAT_MAX", "lower"], 2))
  expect_equal(res$MFO_upper[1], round(r$ci["MFO", "upper"], 4))
  res0 <- suppressWarnings(suppressMessages(.mfos("folder", root)))
  expect_true(all(is.na(res0$FAT_MAX_lower)))
})

test_that("MFOs() passes sin_fit and reports the SIN limits", {
  root <- withr::local_tempdir()
  parts <- .participants()
  .write_folder(root, parts)
  res <- suppressWarnings(suppressMessages(.mfos("folder", root, sin_fit = "absolute",
                                                 ci = TRUE, B = 50, seed = 2, sin_ci = TRUE)))
  expect_equal(res$sin_fit, c("absolute", "absolute"))
  expect_false(anyNA(res$MFO_SIN))
  expect_false(anyNA(res$Fatmin_lower))
  # without sin_ci the SIN has no interval, MFO() still has one
  res1 <- suppressWarnings(suppressMessages(.mfos("folder", root, ci = TRUE, B = 50, seed = 2)))
  expect_true(all(is.na(res1$Fatmin_lower)))
  expect_false(anyNA(res1$FAT_MAX_lower))
  res0 <- suppressWarnings(suppressMessages(.mfos("folder", root)))
  expect_true(all(is.na(res0$MFO_SIN)))
  expect_equal(res0$sin_fit, c("normalised", "normalised"))
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

test_that("MFOs() saves plots and results in output_dir in xlsx, csv and rds", {
  root <- withr::local_tempdir()
  .write_folder(root, .participants()["P01"])
  out <- withr::local_tempdir()

  for (fmt in c("xlsx", "csv", "rds")) {
    res <- suppressWarnings(suppressMessages(
      MFOs(from = "folder", path = root, db_basal_name = "basal_df",
           db_MFO_name = "MFO_df", db_graded_name = "VO2max_df", step_time = 20,
           cv_var = "RER", author = "Frayn", energy_equivalents = "classic",
           save_plot = fmt == "xlsx", save_result = TRUE, output_format = fmt,
           output_dir = out)))
    expect_true(file.exists(file.path(out, paste0("MFO_result.", fmt))))
  }
  expect_true(file.exists(file.path(out, "MFO_plots", "P01_MFO.png")))
  expect_true(file.exists(file.path(out, "MFO_plots", "P01_kinetics.png")))
  expect_equal(readRDS(file.path(out, "MFO_result.rds")), res)
})

test_that("MFOs() writes no files without output_dir (CRAN policy)", {
  root <- withr::local_tempdir()
  .write_folder(root, .participants())
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  # the message is shown once per session
  old <- .mfo_state$mfos_output_message_shown
  .mfo_state$mfos_output_message_shown <- NULL
  withr::defer(.mfo_state$mfos_output_message_shown <- old)
  run <- function() {
    suppressWarnings(MFOs(from = "folder", path = root, db_basal_name = "basal_df",
                          db_MFO_name = "MFO_df", db_graded_name = "VO2max_df",
                          step_time = 20, cv_var = "RER", author = "Frayn",
                          energy_equivalents = "classic", age = 30))
  }
  msgs <- character(0)
  res <- withCallingHandlers(run(), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_equal(nrow(res), 2)
  expect_equal(list.files(wd, recursive = TRUE, all.files = TRUE, no.. = TRUE),
               character(0))
  expect_equal(sum(grepl("writes no files", msgs)), 1)
  expect_match(msgs[grepl("writes no files", msgs)], "output_dir")
  msgs <- character(0)
  withCallingHandlers(run(), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_false(any(grepl("writes no files", msgs)))
  expect_equal(list.files(wd, recursive = TRUE, all.files = TRUE, no.. = TRUE),
               character(0))
})

test_that("MFOs() writes plots and results only in output_dir", {
  root <- withr::local_tempdir()
  .write_folder(root, .participants())
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  out <- file.path(withr::local_tempdir(), "MFO_output")
  res <- suppressWarnings(suppressMessages(
    MFOs(from = "folder", path = root, db_basal_name = "basal_df",
         db_MFO_name = "MFO_df", db_graded_name = "VO2max_df", step_time = 20,
         cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30,
         output_format = "csv", output_dir = out)))
  expect_true(file.exists(file.path(out, "MFO_result.csv")))
  expect_equal(nrow(utils::read.csv(file.path(out, "MFO_result.csv"))), nrow(res))
  expect_setequal(list.files(file.path(out, "MFO_plots")),
                  paste0(rep(res$ID, each = 2), c("_MFO.png", "_kinetics.png")))
  expect_equal(list.files(wd, all.files = TRUE, no.. = TRUE), character(0))
  expect_error(.mfos("folder", root, output_dir = ""), "output_dir")
})
