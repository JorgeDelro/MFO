# Shared by the app tests (#24): an analysis of the example data as the app
# stores it
.app_test_analysis <- function(...) {
  cart <- .app_example_data()
  o <- utils::modifyList(.app_default_opts(), list(age = 30, ...))
  a <- suppressWarnings(suppressMessages(.app_run_analysis(cart, o)))
  c(a, list(cart = cart, opts = o, run = 1, date = as.POSIXct("2026-11-20 10:00"),
            log = list(messages = "a note", warnings = "a warning", error = NULL)))
}
