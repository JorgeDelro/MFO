# read_cart() and the importer pipeline (SPEC 16). Expected values are
# computed by hand from synthetic breaths, or from the example data of MFO
# turned into breaths; comparisons are exact unless a tolerance is stated.

test_that("breaths are averaged per interval and empty intervals kept as NA", {
  b <- data.frame(Time = c(1, 4, 12, 35), VO2 = c(1000, 2000, 3000, 4000),
                  VCO2 = c(800, 1600, 2400, 3200))
  out <- .bin_breaths(b, 10)
  expect_equal(out$Time, c(10, 20, 30, 40))
  expect_equal(out$VO2, c(1500, 3000, NA, 4000))
  expect_equal(out$VCO2, c(1200, 2400, NA, 3200))
})

test_that("VO2 units: mL/min only above 100 automatically, never guessed below", {
  b <- data.frame(VO2 = c(2.4, 2.6), VCO2 = c(2.0, 2.2))
  expect_error(.to_ml_min(b, "auto", NULL), "median VO2 is 2.5: set vo2_unit")
  expect_equal(.to_ml_min(b, "L/min", NULL)$breaths$VO2, c(2400, 2600))
  expect_equal(.to_ml_min(b, "L/min", NULL)$breaths$VCO2, c(2000, 2200))
  r <- data.frame(VO2 = c(30, 40), VCO2 = c(25, 35))
  expect_error(.to_ml_min(r, "auto", NULL), "median VO2 is 35")
  expect_error(.to_ml_min(r, "mL/kg/min", NULL), "needs body_mass")
  expect_equal(.to_ml_min(r, "mL/kg/min", 70)$breaths$VO2, c(2100, 2800))
  m <- data.frame(VO2 = c(2400, 2600), VCO2 = c(2000, 2200))
  expect_equal(.to_ml_min(m, "auto", NULL)$unit, "mL/min")
  expect_equal(.to_ml_min(m, "auto", NULL)$breaths, m)
})

test_that("RER is VCO2/VO2 and an exported RER off by > 0.02 is counted", {
  # 3 intervals of 10 s; exported RER right in the first two, 0.05 off in the third
  b <- data.frame(Time = c(5, 15, 25), VO2 = c(1000, 2000, 2500),
                  VCO2 = c(800, 1800, 2500), RER = c(0.80, 0.90, 0.95))
  expect_warning(cart <- .cart_from_breaths(b, 10, NULL, "MFO", NULL, "auto", "test"),
                 "more than 0.02 in 1 of 3 intervals")
  expect_equal(cart$db_MFO$RER, c(0.8, 0.9, 1.0))
  expect_equal(cart$info$RER_mismatch, 1)
  expect_equal(names(cart$db_MFO), c("Time", "VO2", "VCO2", "RER"))
  expect_output(print(cart), "exported RER off by > 0.02 in 1 interval")
  # without an exported RER there is nothing to compare
  cart2 <- .cart_from_breaths(b[, 1:3], 10, NULL, "graded", NULL, "auto", "test")
  expect_true(is.na(cart2$info$RER_mismatch))
  expect_null(cart2$db_MFO)
  expect_equal(nrow(cart2$db_graded), 3)
})

test_that("cuts split the recording into phases", {
  binned <- data.frame(Time = seq(10, 100, 10), VO2 = 1:10)
  p <- .split_phases(binned, c(basal_end = 30, MFO_end = 70), "MFO", 10)
  expect_equal(p$basal$VO2, 1:3)
  expect_equal(p$MFO$VO2, 4:7)
  expect_equal(p$graded$VO2, 8:10)
  p <- .split_phases(binned, c(MFO_end = 70), "MFO", 10)
  expect_null(p$basal)
  expect_equal(p$MFO$VO2, 1:7)
  p <- .split_phases(binned, c(basal_end = 30), "MFO", 10)
  expect_equal(p$MFO$VO2, 4:10)
  expect_null(p$graded)
  expect_message(p <- .split_phases(binned, c(MFO_end = 72), "MFO", 10),
                 "MFO_end = 70")
  expect_error(.split_phases(binned, c(70), "MFO", 10), "named")
  expect_error(.split_phases(binned, c(end = 70), "MFO", 10), "named")
  expect_error(.split_phases(binned, c(basal_end = 70, MFO_end = 30), "MFO", 10),
               "increasing")
  expect_error(.split_phases(binned, c(MFO_end = 200), "MFO", 10), "inside")
  expect_equal(.split_phases(binned, NULL, "basal", 10)$basal, binned)
})

test_that("pipeline input is checked", {
  b <- data.frame(Time = c(5, 15), VO2 = c(1000, 2000), VCO2 = c(800, 1800))
  expect_error(.cart_from_breaths(b, 0, NULL, "MFO", NULL, "auto", "t"), "interval")
  expect_error(.cart_from_breaths(b[, -3], 10, NULL, "MFO", NULL, "auto", "t"),
               "no column\\(s\\) VCO2")
  expect_error(.cart_from_breaths(transform(b, Time = NA), 10, NULL, "MFO", NULL,
                                  "auto", "t"), "no breaths")
})

# The three example tables (20-s records) turned into 4 breaths per record at
# t - 17.5, t - 12.5, t - 7.5 and t - 2.5 s (inside the interval [t - 20, t))
# with the record's values, joined into one
# recording. read_cart's pipeline with 20-s intervals and the two cuts must
# give back the tables, and MFO() the same results as with the tables.
.as_breaths <- function() {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  cols <- c("VO2", "VCO2", "HR", "Load")
  basal <- data.frame(basal_df)[, c("VO2", "VCO2", "HR")]
  basal$Load <- 0
  tab <- rbind(basal, data.frame(MFO_df)[, cols], data.frame(VO2max_df)[, cols])
  rec_end <- seq_len(nrow(tab)) * 20
  idx <- rep(seq_len(nrow(tab)), each = 4)
  out <- tab[idx, ]
  out$Time <- rec_end[idx] - rep(c(17.5, 12.5, 7.5, 2.5), nrow(tab))
  list(breaths = out, n_basal = nrow(basal_df), n_MFO = nrow(MFO_df))
}

test_that("a joined recording read with cuts reproduces MFO() on the tables", {
  x <- .as_breaths()
  cuts <- c(basal_end = x$n_basal * 20, MFO_end = (x$n_basal + x$n_MFO) * 20)
  cart <- .cart_from_breaths(x$breaths, 20, cuts, "MFO", NULL, "auto", "test")
  expect_s3_class(cart, "MFO_cart")
  expect_equal(cart$step_time, 20)
  expect_equal(cart$info$empty_intervals, 0)
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  expect_equal(cart$db_MFO$VO2, MFO_df$VO2)
  expect_equal(cart$db_MFO$Load, MFO_df$Load)
  run <- function(...) suppressWarnings(suppressMessages(
    MFO(cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30, ...)))
  r_cart <- run(step_time = cart$step_time, db_MFO = cart$db_MFO,
                db_basal = cart$db_basal, db_graded = cart$db_graded)
  r_tab <- run(step_time = 20, db_MFO = MFO_df, db_basal = basal_df,
               db_graded = VO2max_df)
  # Substrates use VO2 and VCO2 only, so MFO, Fatmax and its load are
  # identical. RER is recomputed as VCO2/VO2 (the tables carry a rounded RER)
  # and the basal window is chosen by the CV of RER: resting FAT differs by
  # 0.002 g/min, hence the tolerance there.
  expect_identical(r_cart$MFO, r_tab$MFO)
  expect_identical(r_cart$FAT_MAX, r_tab$FAT_MAX)
  expect_identical(r_cart$VO2max, r_tab$VO2max)
  expect_identical(r_cart$FAT_MAX_load, r_tab$FAT_MAX_load)
  expect_equal(r_cart$x_FAT, r_tab$x_FAT, tolerance = 0.003 / r_tab$x_FAT)
  expect_output(print(cart), "basal +45 intervals")
})

test_that("read_cart() reads a ZAN export through spiro", {
  skip_if_not_installed("spiro")
  f <- spiro::spiro_example("zan_gxt")
  cart <- read_cart(f, phase = "graded")
  raw <- spiro::spiro_raw(f)
  bin <- floor(raw$time / 10)
  # interval 101 (1000-1010 s) is the mean of its breaths
  expect_equal(cart$db_graded$VO2[101], mean(raw$VO2[bin == 100]))
  expect_equal(cart$db_graded$Load[101], mean(raw$load[bin == 100]))
  expect_equal(cart$info$n_breaths, nrow(raw))
  expect_equal(cart$info$vo2_unit, "mL/min")
  expect_null(cart$db_MFO)
  expect_output(print(cart), "graded +301 intervals")
  cart2 <- read_cart(f, device = "zan", cuts = c(MFO_end = 1500))
  expect_equal(nrow(cart2$db_MFO), 150)
  expect_equal(cart2$info$device, "zan")
  expect_error(read_cart("no_such_file.csv"), "File not found")
  expect_error(read_cart(f, device = "parvo"))
})


# --- device = "generic" (#22). No real export yet: the example tables are
# written as an analyser would (metadata line, header, units row, decimal
# comma, mm:ss, VO2 in L/min) and must be read back exactly ---

.write_generic_csv <- function(db, file, time = seq_len(nrow(db)) * 20) {
  comma <- function(x) sub(".", ",", format(x, digits = 15, trim = TRUE), fixed = TRUE)
  writeLines(c("Exported by a cart;MFO test",
               "t;VO2;VCO2;FC;Watts",
               "mm:ss;L/min;L/min;1/min;W",
               sprintf("%s;%s;%s;%s;%s",
                       if (max(time) < 3600) sprintf("%02d:%02d", time %/% 60, time %% 60)
                       else sprintf("%02d:%02d:%02d", time %/% 3600, time %% 3600 %/% 60,
                                    time %% 60),
                       comma(db$VO2 / 1000), comma(db$VCO2 / 1000), comma(db$HR),
                       if (is.null(db$Load)) "" else db$Load)), file)
}

test_that("a generic csv with metadata, units row and decimal comma is read exactly", {
  data(MFO_df, package = "MFO", envir = environment())
  f <- withr::local_tempfile(fileext = ".csv")
  .write_generic_csv(MFO_df, f)
  expect_message(
    cart <- read_cart(f, device = "generic", interval = "none", skip = 1,
                      units_row = TRUE, decimal = ",",
                      columns = c(Time = "t", HR = "FC", Load = "Watts")),
    "unit taken from the units row: L/min")
  expect_s3_class(cart, "MFO_cart")
  expect_equal(cart$step_time, 20)
  expect_equal(cart$db_MFO$Time, seq_len(nrow(MFO_df)) * 20)
  # L/min x 1000 back to mL/min (tolerance for the binary representation)
  expect_equal(cart$db_MFO$VO2, MFO_df$VO2, tolerance = 1e-12)
  expect_equal(cart$db_MFO$VCO2, MFO_df$VCO2, tolerance = 1e-12)
  expect_equal(cart$db_MFO$HR, MFO_df$HR)
  expect_equal(cart$db_MFO$Load, MFO_df$Load)
  expect_equal(cart$db_MFO$RER, MFO_df$VCO2 / MFO_df$VO2, tolerance = 1e-12)
  expect_equal(cart$info$vo2_unit, "L/min")
  expect_output(print(cart), "20-s records as exported")
  # an explicit vo2_unit wins over the units row, without the message
  expect_no_message(read_cart(f, device = "generic", interval = "none", skip = 1,
                              units_row = TRUE, decimal = ",", vo2_unit = "L/min",
                              columns = c(Time = "t", HR = "FC", Load = "Watts")))
})

test_that("an xlsx with one sheet per phase reproduces MFO() on the tables", {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO", envir = environment())
  sec <- function(db) seq_len(nrow(db)) * 20
  tab <- function(db) {
    out <- data.frame(Time = sec(db) / 86400, VO2 = db$VO2, VCO2 = db$VCO2, HR = db$HR)
    if (!is.null(db$Load)) out$Load <- db$Load
    out
  }
  f <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Rest = tab(basal_df), Steps = tab(MFO_df), GXT = tab(VO2max_df)), f)
  expect_message(
    cart <- read_cart(f, device = "generic", interval = "none",
                      sheets = c(basal = "Rest", MFO = "Steps", graded = "GXT")),
    "Excel times")
  expect_equal(cart$step_time, 20)
  expect_equal(cart$db_basal$Time, sec(basal_df), tolerance = 1e-9)
  expect_equal(cart$db_graded$VO2, VO2max_df$VO2)
  expect_equal(cart$info$n_breaths, nrow(basal_df) + nrow(MFO_df) + nrow(VO2max_df))
  run <- function(...) suppressWarnings(suppressMessages(
    MFO(cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30, ...)))
  r_cart <- run(step_time = cart$step_time, db_MFO = cart$db_MFO,
                db_basal = cart$db_basal, db_graded = cart$db_graded)
  # The tables with RER = VCO2/VO2, as the contract recomputes it
  rer <- function(db) transform(db, RER = VCO2 / VO2)
  r_tab <- run(step_time = 20, db_MFO = rer(MFO_df), db_basal = rer(basal_df),
               db_graded = rer(VO2max_df))
  expect_identical(r_cart$MFO, r_tab$MFO)
  expect_identical(r_cart$FAT_MAX, r_tab$FAT_MAX)
  expect_identical(r_cart$VO2max, r_tab$VO2max)
  expect_identical(r_cart$x_FAT, r_tab$x_FAT)
  expect_identical(r_cart$FAT_MAX_load, r_tab$FAT_MAX_load)
  # one unnamed sheet is the phase named in `phase`
  one <- suppressMessages(read_cart(f, device = "generic", interval = "none",
                                    sheets = "GXT", phase = "graded"))
  expect_equal(one$db_graded$VO2, VO2max_df$VO2)
  expect_null(one$db_MFO)
})

test_that("generic records with cuts split like the shared pipeline", {
  data(list = c("basal_df", "MFO_df"), package = "MFO", envir = environment())
  both <- rbind(data.frame(basal_df)[, c("VO2", "VCO2", "HR")],
                data.frame(MFO_df)[, c("VO2", "VCO2", "HR")])
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Time = seq_len(nrow(both)) * 20, both), f, row.names = FALSE)
  cart <- read_cart(f, device = "generic", interval = "none",
                    cuts = c(basal_end = nrow(basal_df) * 20))
  expect_equal(cart$db_basal$VO2, basal_df$VO2)
  expect_equal(cart$db_MFO$VO2, MFO_df$VO2)
  expect_equal(cart$db_MFO$Time[1], (nrow(basal_df) + 1) * 20)
})

test_that("a generic breath-by-breath file is averaged per interval", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Time = c(1, 4, 12, 35), VO2 = c(1000, 2000, 3000, 4000),
                              VCO2 = c(800, 1600, 2400, 3200)), f, row.names = FALSE)
  cart <- read_cart(f, device = "generic", interval = 10)
  expect_equal(cart$db_MFO$VO2, c(1500, 3000, NA, 4000))
  expect_error(read_cart(f, device = "generic", interval = "none"),
               "regular step.*Time goes from 4 to 12 s")
})

test_that("generic input errors name what to fix", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("time,VO2 (ml/min),VCO2", "20,\"2,4\",2000", "40,2500,2100"), f)
  expect_error(read_cart(f, device = "generic", interval = "none",
                         columns = c(Time = "Tiempo")),
               "\"Tiempo\", \"VO2\" not found.*Columns in the file: \"time\", \"VO2 \\(ml/min\\)\"")
  expect_error(read_cart(f, device = "generic", interval = "none",
                         columns = c(Time = "time", VO2 = "VO2 (ml/min)")),
               "VO2 has 1 value\\(s\\) that are not numbers \\(e.g. \"2,4\"\\).*decimal = \",\"")
  expect_error(read_cart(f, device = "generic", columns = c(Tiempo = "t")),
               "Unknown name\\(s\\) in columns: Tiempo")
  expect_error(read_cart(f, device = "generic", columns = "time"), "named character")
  expect_error(read_cart(f, device = "generic", sheets = c(MFO = "A")), "only for xlsx")
  expect_error(read_cart(f, device = "zan", columns = c(Time = "time")),
               "for device = \"generic\"")
  g <- withr::local_tempfile(fileext = ".dat")
  writeLines("x", g)
  expect_error(read_cart(g, device = "generic"), "not .dat")
})

test_that("sheets are checked against cuts, names and steps", {
  data(MFO_df, package = "MFO", envir = environment())
  d20 <- data.frame(Time = seq_len(10) * 20, VO2 = MFO_df$VO2[1:10], VCO2 = MFO_df$VCO2[1:10])
  d10 <- transform(d20, Time = seq_len(10) * 10)
  f <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(A = d20, B = d10), f)
  expect_error(read_cart(f, device = "generic", interval = "none",
                         sheets = c(MFO = "A"), cuts = c(MFO_end = 100)), "cuts or sheets")
  # one unnamed sheet is the recording, split by cuts
  one <- read_cart(f, device = "generic", interval = "none", sheets = "A",
                   cuts = c(MFO_end = 100))
  expect_equal(one$db_MFO$VO2, d20$VO2[1:5])
  expect_equal(one$db_graded$VO2, d20$VO2[6:10])
  expect_error(read_cart(f, device = "generic", interval = "none",
                         sheets = c(test = "A")), "named basal, MFO")
  expect_error(read_cart(f, device = "generic", interval = "none", sheets = c("A", "B")),
               "Name the sheets")
  expect_error(read_cart(f, device = "generic", interval = "none",
                         sheets = c(MFO = "A", graded = "B")), "different steps")
})


test_that("clock time: error without time_origin = \"clock\", offset kept with it", {
  data(MFO_df, package = "MFO", envir = environment())
  f <- withr::local_tempfile(fileext = ".csv")
  # records every 20 s from 10:32:20 (time of day, hh:mm:ss)
  .write_generic_csv(MFO_df, f, time = 10 * 3600 + 32 * 60 + seq_len(nrow(MFO_df)) * 20)
  args <- list(f, device = "generic", interval = "none", skip = 1, units_row = TRUE,
               decimal = ",", columns = c(Time = "t", HR = "FC", Load = "Watts"))
  expect_error(suppressMessages(do.call(read_cart, args)),
               "first Time is 37940 s \\(10:32:20\\).*time_origin = \"clock\"")
  cart <- suppressMessages(do.call(read_cart, c(args, time_origin = "clock")))
  expect_equal(cart$info$time_origin, "clock")
  expect_equal(cart$info$time_offset, 37940)
  expect_equal(cart$db_MFO$Time, (seq_len(nrow(MFO_df)) - 1) * 20)
  expect_equal(cart$step_time, 20)
  expect_equal(cart$db_MFO$VO2, MFO_df$VO2, tolerance = 1e-12)
  # elapsed time keeps its values and records offset 0
  g <- withr::local_tempfile(fileext = ".csv")
  .write_generic_csv(MFO_df, g)
  e <- suppressMessages(read_cart(g, device = "generic", interval = "none", skip = 1,
                                  units_row = TRUE, decimal = ",",
                                  columns = c(Time = "t", HR = "FC", Load = "Watts")))
  expect_equal(c(e$info$time_origin, e$info$time_offset), c("elapsed", "0"))
  expect_error(read_cart(g, device = "zan", time_origin = "clock"), "for device = \"generic\"")
})
