# Expected percentiles: linear interpolation by hand in Table 1 of
# Amaro-Gahete, Sanchez-Delgado and Ruiz (2018), Front Physiol 9:1460
# (tolerance 1e-10). Obesity ranges: Chavez-Guevara et al. (2023), SPEC 19.

fake_MFO <- function(MFO = 0.32, FAT_MAX = 40, mode = "walking", author = "Frayn",
                     HRmax = NA_real_, HR_peak = 190, type = "step",
                     step_duration = 180, steady_state_duration = 60) {
  list(MFO_db = data.frame(), MFO = MFO, FAT_MAX = FAT_MAX,
       FatMax_zone_lower = 30, FatMax_zone_upper = 50,
       FAT_MAX_HR = 120, FatMax_zone_lower_HR = 110, FatMax_zone_upper_HR = 130,
       HRmax = HRmax, HR_peak = HR_peak, mode = mode, author = author,
       method_used = "P3",
       protocol = list(type = type, step_duration = step_duration,
                       steady_state_duration = steady_state_duration))
}

test_that("percentiles interpolate between P20 and P80 without extrapolation", {
  # Obese group: MFO P40 0.30, P60 0.39; Fatmax P20 32.3, P40 39.1
  z <- MFO_zones(fake_MFO(MFO = 0.32, FAT_MAX = 36.2), weight_status = "obese")
  expect_equal(z$MFO_percentile, 40 + 20 * 0.02 / 0.09, tolerance = 1e-10)
  expect_equal(z$Fatmax_percentile, 20 + 20 * 3.9 / 6.8, tolerance = 1e-10)
  expect_equal(z$MFO_percentile_label, "P44")
  expect_equal(z$reference_n, 29L)
  expect_match(z$reference, "Obese sedentary adults")

  # All: MFO P20 0.24, P80 0.42 (limits included)
  pr <- function(m) MFO_zones(fake_MFO(MFO = m))
  expect_equal(pr(0.24)$MFO_percentile, 20, tolerance = 1e-10)
  expect_equal(pr(0.42)$MFO_percentile, 80, tolerance = 1e-10)
  expect_identical(pr(0.23)$MFO_percentile_label, "< P20")
  expect_true(is.na(pr(0.23)$MFO_percentile))
  expect_identical(pr(0.43)$MFO_percentile_label, "> P80")
  expect_true(is.na(pr(0.43)$MFO_percentile))
})

test_that("reference groups follow the combinations of Table 1", {
  x <- fake_MFO()
  expect_equal(MFO_zones(x)$reference_n, 167L)
  expect_equal(MFO_zones(x, sex = "female")$reference_n, 107L)
  expect_equal(MFO_zones(x, age_group = "middle-aged")$reference_n, 42L)
  expect_equal(MFO_zones(x, sex = "male", age_group = "young")$reference_n, 41L)
  expect_error(MFO_zones(x, sex = "male", weight_status = "obese"),
               "does not cross weight_status")
  expect_error(MFO_zones(x, age_group = "young", weight_status = "normal"),
               "does not cross weight_status")
  expect_error(MFO_zones(x, sex = "man"), "sex must be one of")
})

test_that("cycling gets no percentile; running does, with the difference stated", {
  expect_message(z <- MFO_zones(fake_MFO(mode = "cycling")), "walking on a treadmill")
  expect_true(is.na(z$MFO_percentile_label))
  expect_true(is.na(z$Fatmax_percentile))
  expect_match(z$reference_differences, "mode: cycling")
  z <- MFO_zones(fake_MFO(mode = "running"))
  expect_false(is.na(z$MFO_percentile_label))
  expect_match(z$reference_differences, "mode: running \\(reference: treadmill walking\\)")
  expect_false(grepl("mode:", MFO_zones(fake_MFO())$reference_differences))
})

test_that("reference_differences declares equation, protocol, method and fasting", {
  z <- MFO_zones(fake_MFO())
  expect_false(grepl("equation|protocol", z$reference_differences))
  expect_match(z$reference_differences, "through \\(0;0\\)")
  expect_match(z$reference_differences, "5-6 h, not overnight")
  z <- MFO_zones(fake_MFO(author = "Jeukendrup", step_duration = 240))
  expect_match(z$reference_differences, "equation: Jeukendrup")
  expect_match(z$reference_differences, "protocol: 240-s steps")
  expect_match(MFO_zones(fake_MFO(type = "ramp"))$reference_differences, "protocol: ramp")
})

test_that("HRmax: given > measured peak > predicted on request", {
  x <- fake_MFO(HR_peak = 190)
  z <- MFO_zones(x)
  expect_equal(c(z$HRmax, z$HRmax_source), c("190", "measured"))
  expect_equal(z$FAT_MAX_porcHRmax, 120 / 190 * 100, tolerance = 1e-10)
  z <- MFO_zones(x, HRmax = 200)
  expect_identical(z$HRmax_source, "given")
  expect_equal(z$zone_upper_porcHRmax, 65, tolerance = 1e-10)
  expect_identical(MFO_zones(fake_MFO(HRmax = 195))$HRmax_source, "given")

  # Tanaka 208 - 0.7 x 40 = 180; Fox 220 - 40 = 180 (Tanaka et al. 2001)
  y <- fake_MFO(HR_peak = NA)
  expect_message(z <- MFO_zones(y), "No HRmax")
  expect_true(is.na(z$FAT_MAX_porcHRmax))
  z <- MFO_zones(y, predicted_HRmax = TRUE, age = 40)
  expect_equal(z$HRmax, 180)
  expect_identical(z$HRmax_source, "predicted (Tanaka)")
  z <- MFO_zones(y, predicted_HRmax = TRUE, age = 50, HRmax_formula = "fox")
  expect_equal(c(z$HRmax, z$HRmax_source), c("170", "predicted (Fox)"))
  expect_error(MFO_zones(y, predicted_HRmax = TRUE), "needs age")
  expect_message(z <- MFO_zones(x, predicted_HRmax = TRUE, age = 40), "takes precedence")
  expect_identical(z$HRmax_source, "measured")
})

test_that("obesity range of Chavez-Guevara et al. (2023)", {
  x <- fake_MFO(HR_peak = 200)  # zone 55-65 % HRmax
  expect_error(MFO_zones(x, population = "obesity"), "needs body_fat")
  z <- MFO_zones(x, population = "obesity", body_fat = 38)
  expect_equal(c(z$obesity_range_lower, z$obesity_range_upper), c(61, 66))
  expect_identical(z$obesity_zone, "partly overlaps")
  z <- MFO_zones(x, population = "obesity", body_fat = 34.9)
  expect_equal(c(z$obesity_range_lower, z$obesity_range_upper), c(57, 64))
  expect_message(z <- MFO_zones(x, population = "obesity", body_fat = 35),
                 "35.0 % is not covered")
  expect_true(all(is.na(c(z$obesity_range_lower, z$obesity_range_upper, z$obesity_zone))))
  expect_true(is.na(MFO_zones(x)$obesity_zone))

  expect_identical(.zone_overlap(58, 63, 57, 64), "inside")
  expect_identical(.zone_overlap(40, 56, 57, 64), "outside")
  expect_identical(.zone_overlap(65, 70, 57, 64), "outside")
  expect_identical(.zone_overlap(NA, 63, 57, 64), NA_character_)

  y <- fake_MFO(HR_peak = NA)
  expect_warning(MFO_zones(y, population = "obesity", body_fat = 40,
                           predicted_HRmax = TRUE, age = 45),
                 "approximate")
})

test_that("MFO_zones() takes MFOs() rows with one value per participant", {
  df <- data.frame(ID = c("P1", "P2"), MFO_g_min = c(0.30, 0.50),
                   FAT_MAX_perVO2max = c(40, 45), FatMax_zone_lower = c(30, 35),
                   FatMax_zone_upper = c(50, 55), FAT_MAX_HR = c(120, 130),
                   FatMax_zone_lower_HR = c(110, 120), FatMax_zone_upper_HR = c(130, 140),
                   HRmax = NA, HR_peak = c(190, NA), mode = "walking", author = "Frayn",
                   method_used = "P3", protocol = "step", stringsAsFactors = FALSE)
  z <- MFO_zones(df, sex = c("male", "female"), predicted_HRmax = TRUE, age = c(30, 40))
  expect_s3_class(z, "MFO_zones")
  expect_equal(z$ID, c("P1", "P2"))
  expect_equal(z$HRmax_source, c("measured", "predicted (Tanaka)"))
  expect_equal(z$reference_n, c(60L, 107L))
  expect_identical(z$MFO_percentile_label[2], "> P80")
  expect_error(MFO_zones(df, sex = c("male", "female", "male")), "length 1 or one value")
  expect_error(MFO_zones(list(a = 1)), "result of MFO")
  expect_output(print(z), "== P2 ==")
})

test_that("MFO() returns HR at the zone limits and the peak HR", {
  data(list = c("MFO_df", "basal_df", "VO2max_df"), package = "MFO", envir = environment())
  res <- suppressMessages(suppressWarnings(
    MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df, db_graded = VO2max_df,
        cv_var = "RER", author = "Frayn")))
  ex <- res$MFO_db[!is.na(res$MFO_db$HR), ]
  expect_equal(res$FatMax_zone_lower_HR,
               approx(ex$porc_VO2, ex$HR, xout = res$FatMax_zone_lower, ties = mean)$y)
  # VO2max window: 30 s rounded up to 2 records of 20 s
  hr2 <- stats::filter(VO2max_df$HR, rep(1 / 2, 2), sides = 1)
  expect_equal(res$HR_peak, max(hr2, na.rm = TRUE))
  expect_identical(res$author, "Frayn")
  expect_true(is.na(res$HRmax))
  z <- suppressMessages(MFO_zones(res))
  expect_identical(z$HRmax_source, "measured")
  expect_output(print(z), "Fatmax zone")
})
