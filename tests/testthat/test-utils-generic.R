# Parsers of the generic reader (#22); expected values by hand, exact

test_that("Time is read from seconds, mm:ss, hh:mm:ss, Excel times and date-times", {
  expect_equal(.parse_time(c("0", "20.5", "40"), "."), c(0, 20.5, 40))
  expect_equal(.parse_time(c("20,5", "40"), ","), c(20.5, 40))
  expect_equal(.parse_time(c("0:20", "01:35", "61:00"), "."), c(20, 95, 3660))
  expect_equal(.parse_time(c("00:01:35", "1:02:03.5", NA), "."), c(95, 3723.5, NA))
  expect_equal(.parse_time(c("00:01:35,5"), ","), 95.5)
  expect_message(x <- .parse_time(c("0.000231481481481481", "0.000462962962962963"), "."),
                 "Excel times")
  expect_equal(x, c(20, 40), tolerance = 1e-9)
  expect_equal(.parse_time(c("1899-12-31 00:00:20", "1899-12-31 00:01:00"), "."), c(20, 60))
  expect_error(.parse_time(c("20", "1 min"), "."), "Time not recognised \\(e.g. \"1 min\"\\)")
  expect_error(.parse_time(c(NA, ""), "."), "empty")
})

test_that("numbers follow the declared decimal mark", {
  expect_equal(.parse_number(c("1.5", "", "NA", "-"), ".", "VO2"), c(1.5, NA, NA, NA))
  expect_equal(.parse_number(c("1,5", "2"), ",", "VO2"), c(1.5, 2))
  expect_error(.parse_number(c("1,5"), ".", "VO2"), "use decimal = \",\"")
  expect_error(.parse_number(c("1.234,5"), ",", "VO2"), "not numbers")
  expect_error(.parse_number(c("abc"), ".", "HR"), "Column HR.*\"abc\"\\)\\.$")
})

test_that("the csv separator is detected from the header line", {
  expect_equal(.detect_sep("a;b;c", "."), ";")
  expect_equal(.detect_sep("a,b,c", "."), ",")
  expect_equal(.detect_sep("a\tb\tc", ","), "\t")
  expect_equal(.detect_sep("a,b;c", ","), ";")
  expect_equal(.detect_sep("abc", ","), ";")
})

test_that("a units row is a declaration; vo2_unit given wins", {
  expect_equal(.unit_from_label("ml/min"), "mL/min")
  expect_equal(.unit_from_label("[L/min]"), "L/min")
  expect_equal(.unit_from_label("mL\u00b7kg/min"), "mL/kg/min")
  expect_true(is.na(.unit_from_label("STPD")))
  expect_true(is.na(.unit_from_label("")))
  expect_equal(.generic_unit("mL/min", "L/min", "L/min"), "mL/min")
  expect_message(u <- .generic_unit("auto", "L/min", "L/min"), "units row: L/min")
  expect_equal(u, "L/min")
  expect_message(u <- .generic_unit("auto", "STPD", ""), "not recognised")
  expect_equal(u, "auto")
  expect_equal(.generic_unit("auto", "", ""), "auto")
  expect_error(.generic_unit("auto", "L/min", "mL/min"), "same unit")
})

test_that("interval = \"none\" needs a regular step", {
  expect_equal(.regular_step(c(20, 40, 60)), 20)
  expect_error(.regular_step(20), "at least two")
  expect_error(.regular_step(c(20, 40, 70)), "from 40 to 70 s \\(record 2 to 3\\)")
  expect_error(.regular_step(c(40, 20)), "regular step")
})
