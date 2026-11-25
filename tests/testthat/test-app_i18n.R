# Strings of the app (#24): one file, every key used exists. English only
# since 2026-11-25; the key -> text lookup stays for another language

test_that("every key used by the app is in i18n.csv", {
  s <- .app_strings()
  expect_equal(names(s), c("key", "en"))
  expect_false(anyDuplicated(s$key) > 0)
  expect_true(all(nzchar(s$en)))
  files <- list.files(test_path("..", "..", "R"), pattern = "^app_.*\\.R$", full.names = TRUE)
  skip_if(length(files) == 0, "R sources not available (installed package)")
  code <- paste(unlist(lapply(files, readLines)), collapse = "\n")
  # keys are string literals "<area>.<name>" (t("...") or lists
  # of keys for the choices of an input)
  used <- unique(gsub("\"", "", unlist(regmatches(
    code, gregexpr("\"(app|tab|data|an|res|zo|tbl|rep|rel)\\.[A-Za-z0-9_.]+\"", code)))))
  expect_true(length(used) > 20)
  expect_equal(setdiff(used, s$key), character(0))
  # cond.* keys word the conditions of the package (test-utils-conditions.R)
  expect_equal(setdiff(grep("^cond\\.", s$key, value = TRUE, invert = TRUE), used),
               character(0))
})

test_that(".tr() falls back to English and then to the key", {
  expect_equal(.tr("tab.data"), "Data")
  expect_equal(.tr("tab.data", "fr"), "Data")
  expect_equal(.tr("no.such.key"), "no.such.key")
})
