# Protocol and analysis tab (#24): the app runs MFO() and MFO_kinetics() with
# the options; expected values are those of MFO() called directly (exact)

test_that("options are turned into the arguments of MFO() and MFO_protocol()", {
  expect_equal(.app_numbers("300, 180"), c(300, 180))
  expect_equal(.app_numbers("300;180 60"), c(300, 180, 60))
  expect_null(.app_numbers(" "))
  expect_null(.app_numbers("abc"))
  expect_null(.app_na_null(NA))
  expect_equal(.app_na_null(70), 70)
  o <- .app_default_opts()
  p <- .app_protocol(o)
  expect_equal(c(p$type, p$mode, p$load_unit), c("step", "cycling", "W"))
  p <- .app_protocol(utils::modifyList(o, list(type = "step_variable",
                                               step_duration = "300, 180")))
  expect_equal(p$step_duration, c(300, 180))
  p <- .app_protocol(utils::modifyList(o, list(type = "relative", load_start = 1,
                                               load_increment = 0.5)))
  expect_equal(p$load_unit, "W/kg")
  p <- .app_protocol(utils::modifyList(o, list(type = "ramp", ramp_window = 30)))
  expect_equal(p$type, "ramp")
})

test_that("the analysis of the app is MFO() and MFO_kinetics() on the cart", {
  cart <- .app_example_data()
  o <- utils::modifyList(.app_default_opts(), list(age = 30, body_mass = 70, HRmax = 190,
                                                   RER_cutoff = "safe"))
  a <- suppressWarnings(suppressMessages(.app_run_analysis(cart, o)))
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = cart$db_MFO, db_basal = cart$db_basal,
        db_graded = cart$db_graded, cv_var = "RER", author = "Frayn",
        energy_equivalents = "classic", age = 30, body_mass = 70, HRmax = 190,
        RER_cutoff = "safe")))
  expect_identical(a$res$MFO, r$MFO)
  expect_identical(a$res$FAT_MAX, r$FAT_MAX)
  expect_identical(a$res$FAT_MAX_porcHRmax, r$FAT_MAX_porcHRmax)
  expect_identical(a$kin$d, suppressWarnings(MFO_kinetics(r$MFO_db))$d)
  # VO2max given instead of the graded test; custom RER cutoff
  o2 <- utils::modifyList(o, list(vo2max_source = "value", VO2max = 3000,
                                  RER_cutoff = "custom", RER_custom = 0.97))
  a2 <- suppressWarnings(suppressMessages(.app_run_analysis(cart, o2)))
  expect_equal(a2$res$VO2max, 3000)
  # intervals on demand
  a3 <- suppressWarnings(suppressMessages(.app_run_analysis(cart, o, ci = "parametric",
                                                            B = 50, seed = 1)))
  expect_equal(rownames(a3$res$ci)[1:2], c("MFO", "FAT_MAX"))
})

test_that("the Analysis module runs on the data and reports a missing input", {
  skip_if_not_installed("shiny")
  cart <- .app_example_data()
  data <- shiny::reactiveVal(NULL)
  shiny::testServer(.mod_analysis_server, args = list(data = data, lang = "en"), {
    session$setInputs(run = 1)
    expect_null(out())
    expect_match(log()$error, "Read the data first")
    data(list(cart = cart))
    session$setInputs(author = "Frayn", method = "P2", age = 30, run = 2)
    expect_equal(out()$res$method_used, "P2")
    expect_equal(out()$opts$method, "P2")
    expect_identical(out()$cart, cart)
    expect_true(length(out()$log$messages) > 0)
    # an error from MFO() is shown, not thrown
    session$setInputs(vo2max_source = "value", VO2max = NA, run = 3)
    expect_null(out())
    expect_match(log()$error, "VO2max")
  })
})
