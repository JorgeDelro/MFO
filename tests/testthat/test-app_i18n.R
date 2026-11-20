# Strings of the app (#24): one file, every key used exists, both languages filled

test_that("every key used by the app is in i18n.csv, in English and Spanish", {
  s <- .app_strings()
  expect_equal(names(s), c("key", "en", "es"))
  expect_false(anyDuplicated(s$key) > 0)
  expect_true(all(nzchar(s$en)))
  expect_true(all(nzchar(s$es)))
  files <- list.files(test_path("..", "..", "R"), pattern = "^app_.*\\.R$", full.names = TRUE)
  skip_if(length(files) == 0, "R sources not available (installed package)")
  code <- paste(unlist(lapply(files, readLines)), collapse = "\n")
  # keys are string literals "<area>.<name>" (t("...") or lists
  # of keys for the choices of an input)
  used <- unique(gsub("\"", "", unlist(regmatches(
    code, gregexpr("\"(app|tab|data|an|res|zo|tbl|rep|rel)\\.[A-Za-z0-9_.]+\"", code)))))
  expect_true(length(used) > 20)
  expect_equal(setdiff(used, s$key), character(0))
  expect_equal(setdiff(s$key, used), character(0))
})

test_that(".tr() falls back to English and then to the key", {
  expect_equal(.tr("tab.data", "es"), "Datos")
  expect_equal(.tr("tab.data", "en"), "Data")
  expect_equal(.tr("tab.data", "fr"), "Data")
  expect_equal(.tr("no.such.key", "es"), "no.such.key")
})

test_that("the language comes from ?lang= or the default", {
  skip_if_not_installed("shiny")
  expect_equal(.app_lang(list(QUERY_STRING = "?lang=es")), "es")
  expect_equal(.app_lang(list(QUERY_STRING = "?lang=fr")), "en")
  expect_equal(.app_lang(list(QUERY_STRING = NULL), default = "es"), "es")
})
