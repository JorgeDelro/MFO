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

test_that("the server starts the Data module and keeps the language", {
  skip_if_not_installed("shiny")
  # MockShinySession warns that session$request is not a real request; the
  # language then falls back to the default, which is what is tested
  withCallingHandlers(shiny::testServer(.app_server, {
    session$setInputs(lang = "en")
    session$setInputs(`data-source` = "example", `data-read` = 1)
    expect_equal(lang, "en")
    expect_equal(data()$cart$step_time, 20)
  }), warning = function(w) {
    if (grepl("session\\$request", conditionMessage(w))) invokeRestart("muffleWarning")
  })
})
