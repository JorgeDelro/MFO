# Classed conditions (SPEC 10; Jorge, 2026-11-24): the warnings and messages
# keep their English text in R and carry a class and their numbers

test_that("warnings and messages carry a class and their fields", {
  w <- tryCatch(.mfo_warning("negative_fat", "2 step(s) with negative fat", n = 2L),
                warning = function(w) w)
  expect_s3_class(w, c("mfo_warning_negative_fat", "mfo_warning", "warning"))
  expect_equal(w$n, 2L)
  expect_equal(conditionMessage(w), "2 step(s) with negative fat")
  m <- tryCatch(.mfo_message("rer_cutoff", "RER cutoff", cutoff = 0.95, n = 1L),
                message = function(m) m)
  expect_s3_class(m, c("mfo_message_rer_cutoff", "mfo_message", "message"))
  expect_equal(m$cutoff, 0.95)
  expect_message(.mfo_message("x", "shown as before"), "^shown as before\n$")
  expect_warning(.mfo_warning("x", "plain text"), "^plain text$")
})

test_that("MFO() signals its warnings with class and data", {
  d <- data.frame(MFO_df)
  w <- NULL
  withCallingHandlers(
    suppressMessages(MFO(step_time = 20, db_MFO = d, db_basal = basal_df,
                         db_graded = VO2max_df, cv_var = "RER", author = "Frayn",
                         energy_equivalents = "classic", RER_cutoff = 0.9)),
    mfo_warning = function(x) {
      w <<- c(w, list(x))
      invokeRestart("muffleWarning")
    })
  m <- tryCatch(MFO(step_time = 20, db_MFO = d, db_basal = basal_df, db_graded = VO2max_df,
                    cv_var = "RER", author = "Frayn", energy_equivalents = "classic",
                    age = 30, RER_cutoff = 0.9),
                mfo_message_rer_cutoff = function(m) m, warning = function(w) NULL)
  expect_s3_class(m, "mfo_message_rer_cutoff")
  expect_equal(m$cutoff, 0.9)
  expect_true(m$n > 0)
})

test_that("every classed condition has a template in English and Spanish", {
  files <- list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  skip_if(length(files) == 0, "R sources not available (installed package)")
  code <- paste(unlist(lapply(files, readLines)), collapse = "\n")
  ids <- unlist(regmatches(code, gregexpr("\\.mfo_(warning|message)\\(\"[a-z0-9_]+\"", code)))
  ids <- sub("^.*\\(\"", "", sub("\"$", "", ids))
  # ids built in the code: paste0("fatmax_", edge, "_step"), the ramp/step
  # choice and the incomplete last step
  ids <- c(setdiff(ids, "x"), "fatmax_first_step", "fatmax_last_step", "min_steps",
           "min_windows", "incomplete_step_used", "incomplete_step_dropped",
           "incomplete_step_short")
  s <- .app_strings()
  keys <- grep("^cond\\.", s$key, value = TRUE)
  expect_setequal(paste0("cond.", unique(ids)), keys)
  marks <- function(x) sort(unique(unlist(regmatches(x, gregexpr("\\{[^}]+\\}", x)))))
  for (k in keys) {
    expect_identical(marks(s$es[s$key == k]), marks(s$en[s$key == k]), info = k)
  }
})

test_that("the app words a classed condition in the interface language", {
  w <- tryCatch(.mfo_warning("p2_extrapolated", "P2 ...", fatmax = 81.234, lower = 30,
                             upper = 75.5), warning = function(w) w)
  expect_equal(.app_condition_text(w, "en"),
               paste0("P2: the vertex (Fatmax 81.2 % VO₂max) lies outside the measured ",
                      "range (30.0-75.5); Fatmax, MFO and their interval are extrapolated."))
  expect_match(.app_condition_text(w, "es"), "Fatmax 81,2 % VO₂max", fixed = TRUE)
  expect_match(.app_condition_text(w, "es"), "(30,0-75,5)", fixed = TRUE)
  n <- tryCatch(.mfo_warning("negative_fat", "x", n = 3L), warning = function(w) w)
  expect_match(.app_condition_text(n, "es"), "^3 escal")
  # conditions without a template keep their message
  expect_equal(.app_condition_text(simpleWarning("other"), "es"), "other")
  expect_equal(.app_condition_text("already text", "es"), "already text")
  l <- .app_capture({
    .mfo_warning("negative_fat", "x", n = 2L)
    message("a note")
    1
  })
  html <- as.character(.app_alerts(l, lang = "es"))
  expect_match(html, "2 escal")
  expect_match(html, "a note")
})
