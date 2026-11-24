# App structure (#24): the interface is built in both languages

test_that("the interface is built in English and Spanish", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  en <- as.character(.app_ui(list(QUERY_STRING = "")))
  expect_match(en, "Protocol and analysis")
  expect_match(en, "Read data")
  es <- as.character(.app_ui(list(QUERY_STRING = "?lang=es")))
  expect_match(es, "Protocolo y análisis")
  expect_match(es, "Leer datos")
  expect_s3_class(.app(), "shiny.appobj")
})

test_that("the fixed strings are marked for the change of language", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  page <- htmltools::renderTags(.app_ui(list(QUERY_STRING = "")))
  expect_match(page$html, 'data-i18n="tab.analysis"', fixed = TRUE)
  expect_match(page$html, 'data-i18n-placeholder="data.no_file"', fixed = TRUE)
  expect_match(page$head, "mfo-lang", fixed = TRUE)
  d <- .app_dictionary("es")
  expect_equal(d[["tab.data"]], "Datos")
  expect_equal(length(d), nrow(.app_strings()))
})

test_that("changing the language keeps the data (no reload)", {
  skip_if_not_installed("shiny")
  # MockShinySession warns that session$request is not a real request; the
  # language then falls back to the default given to the server
  app <- new.env()
  withCallingHandlers(shiny::testServer(function(input, output, session) {
    app$r <- .app_server(input, output, session, default_lang = "es")
  }, {
    lang <- app$r$lang
    data <- app$r$data
    expect_equal(lang(), "es")
    session$setInputs(lang = "es")
    session$setInputs(`data-source` = "example", `data-read` = 1)
    expect_equal(data()$cart$step_time, 20)
    # Jorge, 2026-11-24: Spanish -> English did nothing
    session$setInputs(lang = "en")
    expect_equal(lang(), "en")
    expect_equal(data()$cart$step_time, 20)
    session$setInputs(lang = "es")
    expect_equal(lang(), "es")
  }), warning = function(w) {
    if (grepl("session\\$request", conditionMessage(w))) invokeRestart("muffleWarning")
  })
})
