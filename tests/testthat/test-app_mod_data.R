# Data tab of the app (#24). The reading is read_cart()'s, so the expected
# values are the example tables of the package (exact)

.example_files <- function(dir) {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO", envir = environment())
  tab <- function(db) {
    out <- data.frame(Time = seq_len(nrow(db)) * 20, VO2 = db$VO2, VCO2 = db$VCO2, HR = db$HR)
    if (!is.null(db$Load)) out$Load <- db$Load
    out
  }
  xlsx <- file.path(dir, "participant.xlsx")
  openxlsx::write.xlsx(list(Rest = tab(basal_df), Steps = tab(MFO_df), GXT = tab(VO2max_df)),
                       xlsx)
  csv <- file.path(dir, "steps.csv")
  utils::write.csv(tab(MFO_df), csv, row.names = FALSE)
  both <- rbind(tab(basal_df)[, c("VO2", "VCO2", "HR")], tab(MFO_df)[, c("VO2", "VCO2", "HR")])
  rec <- file.path(dir, "recording.csv")
  utils::write.csv(data.frame(Time = seq_len(nrow(both)) * 20, both), rec, row.names = FALSE)
  list(xlsx = xlsx, csv = csv, rec = rec, n_basal = nrow(basal_df), MFO_df = MFO_df,
       VO2max_df = VO2max_df, basal_df = basal_df)
}

.generic_settings <- function(...) {
  utils::modifyList(list(device = "generic", columns = NULL, skip = 0, units_row = FALSE,
                         decimal = ".", time_origin = "elapsed", interval = "none",
                         vo2_unit = "auto", body_mass = NULL), list(...))
}

test_that("uploaded files become tables: xlsx sheets and zip contents", {
  dir <- withr::local_tempdir()
  x <- .example_files(dir)
  files <- data.frame(name = c("participant.xlsx", "steps.csv"), datapath = c(x$xlsx, x$csv))
  tabs <- .app_tables(files)
  expect_equal(tabs$label, c("participant.xlsx [Rest]", "participant.xlsx [Steps]",
                             "participant.xlsx [GXT]", "steps.csv"))
  expect_equal(tabs$sheet, c("Rest", "Steps", "GXT", NA))
  expect_equal(nrow(.app_tables(files, expand_sheets = FALSE)), 2)
  zip <- file.path(dir, "all.zip")
  withr::with_dir(dir, utils::zip(zip, c("participant.xlsx", "steps.csv"), flags = "-q"))
  tz <- .app_tables(data.frame(name = "all.zip", datapath = zip))
  expect_equal(tz$label, tabs$label)
  expect_equal(nrow(.app_tables(NULL)), 0)
})

test_that("one table per phase gives the example tables", {
  dir <- withr::local_tempdir()
  x <- .example_files(dir)
  tabs <- .app_tables(data.frame(name = "participant.xlsx", datapath = x$xlsx))
  cart <- .app_read_data(tabs, list(mode = "phases", basal = "participant.xlsx [Rest]",
                                    MFO = "participant.xlsx [Steps]",
                                    graded = "participant.xlsx [GXT]"),
                         .generic_settings())
  expect_s3_class(cart, "MFO_cart")
  expect_equal(cart$step_time, 20)
  expect_equal(cart$db_MFO$VO2, x$MFO_df$VO2)
  expect_equal(cart$db_graded$Load, x$VO2max_df$Load)
  expect_equal(cart$db_basal$HR, x$basal_df$HR)
  # graded test optional; MFO test required
  only <- .app_read_data(tabs, list(mode = "phases", basal = "", MFO = "participant.xlsx [Steps]",
                                    graded = NA), .generic_settings())
  expect_null(only$db_basal)
  expect_error(.app_read_data(tabs, list(mode = "phases", MFO = ""), .generic_settings()),
               "table of the MFO test")
  expect_error(.app_read_data(tabs, list(mode = "phases", MFO = "nope"), .generic_settings()),
               "not found")
})

test_that("one recording with cuts is split as read_cart() splits it", {
  dir <- withr::local_tempdir()
  x <- .example_files(dir)
  tabs <- .app_tables(data.frame(name = "recording.csv", datapath = x$rec))
  cart <- .app_read_data(tabs, list(mode = "cuts", table = "recording.csv",
                                    cuts = c(basal_end = x$n_basal * 20, MFO_end = NA)),
                         .generic_settings())
  expect_equal(cart$db_basal$VO2, x$basal_df$VO2)
  expect_equal(cart$db_MFO$VO2, x$MFO_df$VO2)
  expect_error(.app_read_data(tabs, list(mode = "cuts", table = "recording.csv",
                                         cuts = c(basal_end = NA, MFO_end = NA)),
                              .generic_settings()), "basal_end and/or MFO_end")
})

test_that("tables with different steps are refused", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.csv")
  b <- file.path(dir, "b.csv")
  utils::write.csv(data.frame(Time = 1:5 * 20, VO2 = 1001:1005, VCO2 = 801:805), a, row.names = FALSE)
  utils::write.csv(data.frame(Time = 1:5 * 10, VO2 = 1001:1005, VCO2 = 801:805), b, row.names = FALSE)
  tabs <- .app_tables(data.frame(name = c("a.csv", "b.csv"), datapath = c(a, b)))
  expect_error(.app_read_data(tabs, list(mode = "phases", MFO = "a.csv", graded = "b.csv"),
                              .generic_settings()), "different steps")
})

test_that("messages, warnings and errors are captured for the alerts", {
  out <- .app_capture({
    message("a note")
    warning("a warning")
    42
  })
  expect_equal(out$value, 42)
  expect_equal(out$messages, "a note")
  expect_equal(out$warnings, "a warning")
  expect_null(out$error)
  err <- .app_capture(stop("bad file"))
  expect_null(err$value)
  expect_equal(err$error, "bad file")
})

test_that("the example data run through MFO() and the plot and summary build", {
  cart <- .app_example_data()
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = cart$step_time, db_MFO = cart$db_MFO, db_basal = cart$db_basal,
        db_graded = cart$db_graded, cv_var = "RER", author = "Frayn")))
  expect_true(r$MFO > 0)
  s <- .app_phase_summary(cart)
  expect_equal(s$Phase, c("Resting measure", "MFO test", "Graded test (VO\u2082max)"))
  expect_equal(s$Records, c(45, 72, nrow(cart$db_graded)))
  expect_s3_class(.app_recording_plot(cart, c(900, NA)), "ggplot")
})

test_that("the Data module reads the example and uploaded files", {
  skip_if_not_installed("shiny")
  dir <- withr::local_tempdir()
  x <- .example_files(dir)
  shiny::testServer(.mod_data_server, {
    session$setInputs(source = "example", read = 1)
    expect_equal(result()$cart$step_time, 20)
    expect_equal(result()$source, "example")
    expect_equal(nrow(result()$cart$db_MFO), 72)

    session$setInputs(source = "generic", assign = "phases",
                      files = data.frame(name = "participant.xlsx", datapath = x$xlsx),
                      interval_mode = "none", vo2_unit = "auto", skip = 0, units_row = FALSE,
                      decimal = ".", time_origin = "elapsed", body_mass = NA,
                      tab_basal = "participant.xlsx [Rest]", tab_MFO = "participant.xlsx [Steps]",
                      tab_graded = "participant.xlsx [GXT]",
                      map_Time = "Time", map_VO2 = "VO2", map_VCO2 = "VCO2", map_RER = "",
                      map_HR = "HR", map_Load = "Load", map_VE = "")
    expect_equal(header(), c("Time", "VO2", "VCO2", "HR", "Load"))
    expect_equal(unname(settings()$columns), c("Time", "VO2", "VCO2", "HR", "Load"))
    session$setInputs(read = 2)
    expect_equal(result()$cart$db_MFO$VO2, x$MFO_df$VO2)
    expect_equal(result()$tables$MFO, "participant.xlsx [Steps]")
    expect_null(log()$error)

    # a cut set by dragging; an error is shown, not thrown
    session$setInputs(assign = "cuts", tab_recording = "participant.xlsx [Steps]",
                      brush = list(xmin = 99.6, xmax = 400.2))
    session$setInputs(basal_end = 100, MFO_end = 400, read = 3)
    expect_equal(result()$cuts, c(basal_end = 100, MFO_end = 400))
    expect_equal(result()$cart$db_MFO$VO2, x$MFO_df$VO2[6:20])
    session$setInputs(basal_end = NA, MFO_end = NA, read = 4)
    expect_null(result())
    expect_match(log()$error, "basal_end and/or MFO_end")
  })
})



test_that("the Data tab shows the tables of each phase before the analysis", {
  skip_if_not_installed("shiny")
  cart <- .app_example_data()
  v <- .app_phase_view(cart, "MFO")
  expect_equal(v[["VO\u2082 (mL/min)"]], cart$db_MFO$VO2)
  expect_equal(v$Time[1], "00:00:20")
  expect_null(.app_phase_view(cart["db_MFO"], "basal"))
  shiny::testServer(.mod_data_server, {
    session$setInputs(source = "example", assign = "phases", read = 1)
    expect_match(output$view_MFO, "<table")
    expect_match(output$view_basal, "<table")
    expect_match(output$view_graded, "<table")
  })
})

test_that("the individual-test template is read and analysed as it is", {
  # Jorge, 2026-11-25: xlsx with one sheet per phase, contract column names,
  # so no mapping is needed
  f <- system.file("app", "templates", "MFO_test_template.xlsx", package = "MFO")
  tabs <- .app_tables(data.frame(name = "MFO_test_template.xlsx", datapath = f))
  expect_equal(tabs$sheet, c("Rest", "MFO", "Graded"))
  cart <- .app_read_data(tabs, list(mode = "phases", basal = tabs$label[1],
                                    MFO = tabs$label[2], graded = tabs$label[3]),
                         .generic_settings())
  expect_equal(cart$step_time, 20)
  ex <- .app_example_data()
  expect_equal(cart$db_MFO$VO2, ex$db_MFO$VO2)
  a <- suppressWarnings(suppressMessages(.app_run_analysis(cart, .app_default_opts())))
  b <- suppressWarnings(suppressMessages(.app_run_analysis(ex, .app_default_opts())))
  expect_equal(a$res$MFO, b$res$MFO)
  expect_equal(a$res$FAT_MAX, b$res$FAT_MAX)
})

test_that("the Data tab offers both templates", {
  skip_if_not_installed("shiny")
  shiny::testServer(.mod_data_server, {
    expect_equal(readxl::excel_sheets(output$template_test), c("Rest", "MFO", "Graded"))
    expect_equal(names(utils::read.csv(output$template_results))[1:4],
                 c("id", "session", "MFO", "Fatmax"))
  })
})
