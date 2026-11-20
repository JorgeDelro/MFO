# Zones and references tab (#24): the app shows MFO_zones() of the current
# result; expected values are MFO_zones() called directly (exact)

.zones_opts <- function(...) {
  utils::modifyList(list(HRmax = NA, predicted_HRmax = FALSE, age = NA,
                         HRmax_formula = "tanaka", population = "general", body_fat = NA,
                         sex = "", age_group = "", weight_status = ""), list(...))
}

test_that("the zones of the app are MFO_zones() with the options of the tab", {
  a <- .app_test_analysis()
  zn <- suppressMessages(.app_run_zones(a$res, .zones_opts(sex = "male", age_group = "young",
                                                           population = "obesity",
                                                           body_fat = 38)))
  ref <- suppressMessages(MFO_zones(a$res, sex = "male", age_group = "young",
                                    population = "obesity", body_fat = 38))
  expect_identical(zn$zone_lower_HR, ref$zone_lower_HR)
  expect_identical(zn$reference_n, 41L)
  expect_identical(zn$obesity_zone, ref$obesity_zone)
  tabs <- .app_zones_tables(zn)
  expect_equal(tabs$zones$bpm, formatC(c(ref$FAT_MAX_HR, ref$zone_lower_HR, ref$zone_upper_HR),
                                       format = "f", digits = 0))
  expect_equal(tabs$reference$Percentile, c("no percentile", "no percentile"))
  expect_true(any(grepl("^mode: cycling", tabs$differences)))
  es <- .app_zones_tables(zn, function(k) .tr(k, "es"))
  expect_equal(es$reference$Percentil, c("sin percentil", "sin percentil"))
  expect_equal(.app_zones_label("partly overlaps", function(k) .tr(k, "es")), "solapa en parte")
  expect_equal(.app_zones_label("measured"), "measured peak HR")
  expect_equal(.app_zones_label(NA), NA)
  expect_error(.app_run_zones(a$res, .zones_opts(sex = "male", weight_status = "obese")),
               "does not cross")
})

test_that("the Zones module uses the analysis HRmax and shows errors", {
  skip_if_not_installed("shiny")
  a <- .app_test_analysis(HRmax = 190)
  results <- shiny::reactiveVal(NULL)
  shiny::testServer(.mod_zones_server, args = list(results = results, lang = "en"), {
    session$setInputs(run = 1)
    expect_match(log()$error, "Run the analysis first")
    results(a)
    session$setInputs(HRmax = 190, predicted_HRmax = FALSE, age = 30,
                      HRmax_formula = "tanaka", obesity = TRUE, body_fat = 35, sex = "",
                      age_group = "", weight_status = "obese", run = 2)
    expect_equal(out()$zones$HRmax_source, "given")
    expect_match(output$hrmax_source, "HRmax 190 bpm \\(given\\)")
    expect_match(output$obesity_out$html, "not exactly 35.0")
    expect_match(output$reference_label, "Obese sedentary adults \\(n = 29\\)")
    session$setInputs(sex = "male", run = 3)
    expect_null(out())
    expect_match(log()$error, "does not cross")
  })
})
