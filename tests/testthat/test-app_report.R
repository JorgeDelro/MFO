# Report and downloads (#24): one self-contained HTML file with the contents
# decided by Jorge (2026-11-19), and the group table as xlsx/csv

test_that("the report holds every part, the footer and no external files", {
  a <- .app_test_analysis(HRmax = 190)
  a$data <- list(source = "generic", cuts = c(basal_end = 900, MFO_end = 2340))
  a$ci_settings <- list(type = "parametric", B = 500, conf = 0.95, seed = 3)
  z <- list(zones = suppressMessages(MFO_zones(a$res, sex = "male", population = "obesity",
                                              body_fat = 38)),
            options = list(population = "obesity"), run = 1)
  f <- withr::local_tempfile(fileext = ".html")
  .app_write_report(f, a = a, zones = z, id = "P01",
                    generated = as.POSIXct("2026-11-20 12:00"))
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  for (x in c("<title>MFO report: P01</title>", "Fat oxidation curve", "Results", "Steps of the fit",
              "SIN kinetics", "Heart-rate zones", "Reference values",
              "How this test differs from the reference test", "Quality alerts", "a warning",
              "a note", "Methods", "References", "Obesity range", "HRmax 190 bpm (given)",
              "Analysis: 2026-11-20 10:00", "Report: 2026-11-20 12:00",
              "Phase cuts: basal_end = 900; MFO_end = 2340 s",
              "Intervals: parametric, B = 500, level 0.95, seed 3", "Data: csv/xlsx file")) {
    expect_true(grepl(x, html, fixed = TRUE), label = x)
  }
  expect_equal(lengths(regmatches(html, gregexpr("<img src=\"data:image/png;base64,", html))), 2)
  expect_false(grepl("<script src=|<link ", html))
  expect_true(grepl("Chávez-Guevara", html, fixed = TRUE))
})

test_that("without intervals or zones the report leaves those parts out", {
  a <- .app_test_analysis()
  a$data <- list(source = "example", cuts = NULL)
  f <- withr::local_tempfile(fileext = ".html")
  .app_write_report(f, a = a)
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Heart-rate zones|Intervals:|<th>Lower</th>", html))
  expect_true(grepl("<title>MFO report</title>", html, fixed = TRUE))
  expect_true(grepl("Phase cuts: none (one table per phase).", html, fixed = TRUE))
})

test_that("the group table is written as xlsx and csv and read back", {
  a <- .app_test_analysis()
  a$data <- list(cuts = c(MFO_end = 1500))
  g <- rbind(.app_group_row("P01", "1", a), .app_group_row("P01", "2", a))
  for (fmt in c("xlsx", "csv")) {
    f <- withr::local_tempfile(fileext = paste0(".", fmt))
    .app_write_group(g, f, fmt)
    back <- .app_read_group(f)
    expect_equal(back$ID, g$ID, label = fmt)
    expect_equal(as.character(back$session), g$session, label = fmt)
    expect_equal(back$MFO_g_min, g$MFO_g_min, label = fmt)
    expect_equal(back$phase_cuts, "MFO_end = 1500" |> rep(2), label = fmt)
  }
})

test_that("the Report module lists the group table and empties it", {
  skip_if_not_installed("shiny")
  a <- .app_test_analysis()
  a$data <- list(cuts = NULL)
  results <- shiny::reactiveVal(a)
  zones <- shiny::reactiveVal(list(zones = NULL, run = 99))
  group <- shiny::reactiveVal(.app_group_row("P01", "1", a))
  shiny::testServer(.mod_report_server, args = list(results = results, zones = zones,
                                                    group = group), {
    expect_null(current_zones())
    expect_match(output$group, "P01")
    session$setInputs(clear = 1)
    expect_null(group())
  })
})
