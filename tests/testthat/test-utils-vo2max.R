# Expected values computed by hand from the toy series below (no tolerance
# beyond floating point: all means are exact decimal fractions).
vo2 <- c(1000, 3000, 3100, 2900, 3500, 2000)

test_that("window equal to step_time returns the single highest record", {
  res <- .vo2max_rolling(vo2, step_time = 20, window = 20)
  expect_equal(res$VO2max, 3500)
  expect_equal(res$window, 20)
  expect_equal(res$n_records, 1L)
})

test_that("30 s with 20-s records rounds up to 2 records (40 s)", {
  # Pairwise means: 2000, 3050, 3000, 3200, 2750 -> max 3200
  expect_message(res <- .vo2max_rolling(vo2, step_time = 20, window = 30),
                 "using 2 record\\(s\\) = 40 s")
  expect_equal(res$VO2max, 3200)
  expect_equal(res$window, 40)
  expect_equal(res$n_records, 2L)
})

test_that("60 s with 20-s records averages 3 records without a message", {
  # Means of 3: 2366.667, 3000, 3166.667, 2800 -> max (3100 + 2900 + 3500) / 3
  expect_silent(res <- .vo2max_rolling(vo2, step_time = 20, window = 60))
  expect_equal(res$VO2max, 9500 / 3)
  expect_equal(res$window, 60)
})

test_that("30 s with 15-s records is an exact multiple (2 records)", {
  expect_silent(res <- .vo2max_rolling(vo2, step_time = 15, window = 30))
  expect_equal(res$VO2max, 3200)
  expect_equal(res$window, 30)
})

test_that("windows containing NA are skipped", {
  # Pairs: (1000,3000)=2000, (3000,NA), (NA,2900), (2900,3500)=3200, (3500,2000)=2750
  res <- .vo2max_rolling(c(1000, 3000, NA, 2900, 3500, 2000),
                         step_time = 20, window = 40)
  expect_equal(res$VO2max, 3200)
})

test_that("invalid windows and too-short tests fail with a clear error", {
  expect_error(.vo2max_rolling(vo2, 20, window = 0), "positive")
  expect_error(.vo2max_rolling(vo2, 20, window = c(30, 60)), "positive")
  expect_error(.vo2max_rolling(c(3000, 3100), 20, window = 60), "fewer than 3")
  expect_error(.vo2max_rolling(c(3000, NA, 3100, NA), 20, window = 40),
               "no run of 2")
})
