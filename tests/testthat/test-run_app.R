# App structure (#24): the interface, in English only (Jorge, 2026-11-25)

test_that("the interface is built", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  en <- as.character(.app_ui(list(QUERY_STRING = "")))
  expect_match(en, "MFO analysis")
  expect_match(en, "Protocol and analysis")
  expect_match(en, "Read data")
  expect_no_match(en, "Espa\u00f1ol")
  expect_s3_class(.app(), "shiny.appobj")
})

test_that("the server starts the Data module", {
  skip_if_not_installed("shiny")
  app <- new.env()
  shiny::testServer(function(input, output, session) {
    app$r <- .app_server(input, output, session)
  }, {
    session$setInputs(`data-source` = "example", `data-read` = 1)
    expect_equal(app$r$data()$cart$step_time, 20)
  })
})
