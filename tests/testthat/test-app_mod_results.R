# Results tab (#24): tables built from MFO() and MFO_kinetics() results, and
# the group table with the MFOs() columns


test_that("the results table shows MFO() values and, when computed, intervals", {
  a <- .app_test_analysis()
  tab <- .app_results_table(a$res)
  expect_equal(tab$value[tab$key == "MFO"], a$res$MFO)
  expect_equal(tab$value[tab$key == "FAT_MAX"], a$res$FAT_MAX)
  expect_false("MFO_relative" %in% tab$key)
  expect_true(all(is.na(tab$lower)))
  r <- suppressWarnings(suppressMessages(.app_run_analysis(a$cart, a$opts, ci = "parametric",
                                                           B = 50, seed = 1)))$res
  tab <- .app_results_table(r)
  expect_equal(tab$lower[tab$key == "MFO"], r$ci["MFO", "lower"])
  expect_equal(tab$upper[tab$key == "FatMax_zone_upper"], r$ci["FatMax_zone_upper", "upper"])
  s <- .app_sin_table(a$kin)
  expect_equal(s$Value[s$Parameter == "d"], format(round(a$kin$d, 3)))
})

test_that("hovering picks the nearest step", {
  a <- .app_test_analysis()
  db <- a$res$MFO_db
  st <- .app_hover_step(db, list(x = db$porc_VO2[3] + 0.1, y = 0.2))
  expect_equal(st$Step, db$Step[3])
  expect_null(.app_hover_step(db, NULL))
})

test_that("participant codes, not names", {
  expect_true(.app_valid_id("P01"))
  expect_true(.app_valid_id("lab-2_S01"))
  expect_true(.app_valid_id(strrep("a", 20)))
  expect_false(.app_valid_id(strrep("a", 21)))
  expect_false(.app_valid_id("Ana Lopez"))
  expect_false(.app_valid_id("ana@uca.es"))
  expect_false(.app_valid_id("Jos\u00e9"))
  expect_false(.app_valid_id(""))
  expect_false(.app_valid_id(c("P01", "P02")))
})

test_that("a group row is the MFOs() row with session and date", {
  a <- .app_test_analysis()
  row <- .app_group_row("P01", "2", a)
  ref <- .mfo_summary_row("P01", a$res, a$kin)
  expect_equal(names(row), c("ID", "session", "analysis_date", names(ref)[-1], "phase_cuts"))
  expect_equal(row$phase_cuts, "")
  expect_equal(.app_cuts_text(c(basal_end = 300, MFO_end = 1800)),
               "basal_end = 300; MFO_end = 1800")
  expect_equal(row$MFO_g_min, ref$MFO_g_min)
  expect_equal(row$session, "2")
  expect_equal(row$analysis_date, "2026-11-20 10:00")
})

test_that("the Results module computes intervals and fills the group table", {
  skip_if_not_installed("shiny")
  a <- .app_test_analysis()
  analysis <- shiny::reactiveVal(a)
  group <- shiny::reactiveVal(NULL)
  shiny::testServer(.mod_results_server, args = list(analysis = analysis, group = group), {
    expect_identical(current()$res$MFO, a$res$MFO)
    session$setInputs(ci = "parametric", B = 50, conf = 0.9, seed = 1, compute_ci = 1)
    expect_false(is.null(current()$res$ci))
    expect_equal(current()$ci_settings$B, 50)
    # a new analysis drops the intervals of the previous one
    analysis(utils::modifyList(a, list(run = 2)))
    session$flushReact()
    expect_null(current()$res$ci)
    session$setInputs(ID = "", add = 1)
    expect_null(group())
    session$setInputs(ID = "Ana Lopez", add = 2)
    expect_null(group())
    session$setInputs(ID = "P01", session_id = "1", add = 3)
    session$setInputs(ID = "P01", session_id = "2", add = 4)
    session$setInputs(ID = "P01", session_id = "2", add = 5)
    expect_equal(nrow(group()), 2)
    expect_equal(group()$session, c("1", "2"))
    expect_match(output$group_n, "2 result")
  })
})
