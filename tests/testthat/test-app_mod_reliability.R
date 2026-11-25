# Reliability tab (#24): MFO_reliability() on the group table; expected values
# are MFO_reliability() called directly (exact)

.group_sessions <- function() {
  set.seed(24)
  n <- 6
  data.frame(ID = rep(sprintf("P%02d", 1:n), 2), session = rep(c("1", "2"), each = n),
             MFO_g_min = c(runif(n, 0.2, 0.5), runif(n, 0.2, 0.5)),
             FAT_MAX_perVO2max = c(runif(n, 35, 60), runif(n, 35, 60)),
             stringsAsFactors = FALSE)
}

test_that("the group table becomes the input of MFO_reliability()", {
  g <- .group_sessions()
  x <- .app_reliability_input(g)
  expect_equal(x$participant, g$ID)
  r <- suppressWarnings(MFO_reliability(x))
  ref <- suppressWarnings(MFO_reliability(transform(g, participant = ID)))
  expect_equal(r$table, ref$table)
  expect_error(.app_reliability_input(NULL), "empty")
  expect_error(.app_reliability_input(g[, -2]), "missing: session")
})

test_that("an uploaded results table with only id, session, MFO and Fatmax works", {
  # Jorge, 2026-11-25: required columns id, session, MFO (g/min), Fatmax
  g <- .group_sessions()
  short <- data.frame(id = g$ID, session = g$session, MFO = g$MFO_g_min,
                      Fatmax = g$FAT_MAX_perVO2max)
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(short, f, row.names = FALSE)
  x <- .app_reliability_input(.app_read_group(f))
  r <- suppressWarnings(MFO_reliability(x))
  ref <- suppressWarnings(MFO_reliability(transform(g, participant = ID)))
  expect_equal(r$table$variable, c("MFO_g_min", "FAT_MAX_perVO2max"))
  expect_equal(r$table$ICC, ref$table$ICC)
  expect_error(.app_reliability_input(short[, c("id", "session", "MFO")]),
               "missing: Fatmax")
  # the template has the required columns and optional ones of MFOs()
  tpl <- utils::read.csv(system.file("app", "templates", "MFO_results_template.csv",
                                     package = "MFO"))
  expect_equal(names(tpl)[1:4], c("id", "session", "MFO", "Fatmax"))
  # 3 participants x 2 sessions, so it runs as a demonstration (Jorge,
  # 2026-11-25)
  expect_equal(nrow(tpl), 6)
  expect_true(all(table(tpl$id) == 2))
  r <- suppressWarnings(MFO_reliability(.app_reliability_input(tpl)))
  expect_equal(unique(r$table$n), 3)
  expect_true(all(c("MFO_g_min", "FAT_MAX_perVO2max", "FatMax_zone_lower") %in%
                    r$table$variable))
})

test_that("the Reliability module uses the session table or an uploaded one", {
  skip_if_not_installed("shiny")
  g <- .group_sessions()
  group <- shiny::reactiveVal(g)
  f <- withr::local_tempfile(fileext = ".xlsx")
  .app_write_group(g, f, "xlsx")
  shiny::testServer(.mod_reliability_server, args = list(group = group), {
    session$setInputs(source = "session", conf = 0.95, pair = 1, run = 1)
    expect_equal(out()$table$variable, c("MFO_g_min", "FAT_MAX_perVO2max"))
    expect_match(log()$warnings[1], "Koo & Li")
    session$setInputs(source = "file", file = NULL, run = 2)
    expect_match(log()$error, "Upload the results table")
    session$setInputs(file = data.frame(name = "MFO_group.xlsx", datapath = f), run = 3)
    expect_equal(out()$table$ICC, suppressWarnings(
      MFO_reliability(.app_reliability_input(g)))$table$ICC)
    group(g[1:2, ])
    session$setInputs(source = "session", run = 4)
    expect_null(out())
    expect_match(log()$error, "At least")
  })
})

test_that("the Reliability tab offers the results template", {
  skip_if_not_installed("shiny")
  group <- shiny::reactiveVal(NULL)
  shiny::testServer(.mod_reliability_server, args = list(group = group), {
    f <- output$template
    expect_equal(names(utils::read.csv(f))[1:4], c("id", "session", "MFO", "Fatmax"))
  })
})
