# Hand calculations: constant resting data VO2 = 300, VCO2 = 240 ml/min
# (0.300 / 0.240 L/min, RER = 0.80); energy 4 kcal/g CHO, 9 kcal/g FAT.
#   Frayn (1983):           CHO = 4.55*0.240 - 3.21*0.300   = 0.12900
#                           FAT = 1.67*0.300 - 1.67*0.240   = 0.10020
#   Peronnet (1991): no printed equation; derived from Table 1, whose
#     constants are those of the Jeukendrup resting pair below, so
#                           CHO = 0.1326538, FAT = 0.1000929
#   Jeukendrup (2005) at rest: 100 % glucose pair from Table 4 constants
#     (glucose 0.7455/0.7426, fatty acid 2.0092/1.4136 L/g),
#     det = 2.0092*0.7426 - 0.7455*1.4136 = 0.43819
#     CHO = (2.0092*0.240 - 1.4136*0.300) / det = 0.1326538
#     FAT = (0.7426*0.300 - 0.7455*0.240) / det = 0.1000929
# Tolerance 1e-10 for the decimal coefficients; 1e-6 for the derived ones
# (hand values rounded to 7 decimals).
rest <- data.frame(VO2 = rep(300, 30), VCO2 = rep(240, 30),
                   RER = rep(0.8, 30), HR = rep(60, 30))

test_that("met_basal defaults to Frayn (v0.2.0 behaviour)", {
  b <- met_basal(step_time = 20, db = rest, cv_var = "VO2")
  expect_equal(b$x_CHO, 0.129, tolerance = 1e-10)
  expect_equal(b$x_FAT, 0.1002, tolerance = 1e-10)
  expect_equal(b$x_Kcal, 0.129 * 4 + 0.1002 * 9, tolerance = 1e-10)
})

test_that("met_basal uses the requested author equations", {
  b_p <- met_basal(20, rest, "VO2", author = "Peronnet")
  expect_equal(b_p$x_CHO, 0.1326538, tolerance = 1e-6)
  expect_equal(b_p$x_FAT, 0.1000929, tolerance = 1e-6)
  expect_equal(b_p$x_Kcal, b_p$x_CHO * 4 + b_p$x_FAT * 9, tolerance = 1e-10)

  # Jeukendrup at rest: 100 % glucose pair, no VO2max needed
  b_j <- met_basal(20, rest, "VO2", author = "Jeukendrup")
  expect_equal(b_j$x_CHO, 0.1326538, tolerance = 1e-6)
  expect_equal(b_j$x_FAT, 0.1000929, tolerance = 1e-6)
})

test_that("substrate_oxidation picks the Jeukendrup set per value", {
  # Coefficients printed in Jeukendrup & Wallis (2005), Table 5
  # 1200 / 3000 = 40 % -> CHO = 4.344*1.020 - 3.061*1.200 = 0.75768
  #                       FAT = 1.695*1.200 - 1.701*1.020 = 0.29898
  # 1800 / 3000 = 60 % -> CHO = 4.210*1.620 - 2.962*1.800 = 1.48860
  #                       FAT = 1.695*1.800 - 1.701*1.620 = 0.29538
  s <- substrate_oxidation(VO2 = c(1200, 1800), VCO2 = c(1020, 1620),
                           author = "Jeukendrup", VO2max = 3000)
  expect_equal(s$CHO, c(0.75768, 1.4886), tolerance = 1e-10)
  expect_equal(s$FAT, c(0.29898, 0.29538), tolerance = 1e-10)

  expect_error(substrate_oxidation(1200, 1020, "Jeukendrup"), "VO2max is required")
})


# Constant steps for calculate_vars(): 1-min steps, 20-s records (3 per step)
make_steps <- function(vo2, rer = 0.85) {
  data.frame(VO2 = rep(vo2, each = 3), VCO2 = rep(vo2 * rer, each = 3),
             RER = rer, HR = rep(seq(100, by = 10, length.out = length(vo2)), each = 3))
}

test_that("Jeukendrup warns once for steps outside 40-75 %VO2max", {
  # VO2max 2600: 1000 -> 38.5 %, 1300 -> 50.0 %, 1950 -> 75.0 %, 2000 -> 76.9 %
  db <- make_steps(c(1000, 1300, 1950, 2000))
  expect_warning(
    calculate_vars(20, db, VO2max = 2600, author = "Jeukendrup",
                   step_duration = 60, steady_state_duration = 60),
    "2 step\\(s\\) outside this range \\(38.5, 76.9 %VO2max\\)")
  # Limits are inclusive: 40 % (1040) and 75 % (1950) do not warn
  expect_no_warning(
    calculate_vars(20, make_steps(c(1040, 1300, 1950)), VO2max = 2600,
                   author = "Jeukendrup", step_duration = 60,
                   steady_state_duration = 60))
  # Other equation sets have no range
  expect_no_warning(
    calculate_vars(20, db, VO2max = 2600, author = "Frayn",
                   step_duration = 60, steady_state_duration = 60))
})

test_that("Jeukendrup range warning ignores steps removed by RER_cutoff", {
  db <- rbind(make_steps(c(1300, 1600)), make_steps(2200, rer = 1.05))
  expect_no_warning(suppressMessages(
    calculate_vars(20, db, VO2max = 2600, author = "Jeukendrup",
                   step_duration = 60, steady_state_duration = 60,
                   RER_cutoff = 1.0)))
})

test_that("the resting row never triggers the Jeukendrup range warning", {
  # Rest at 300 ml/min = 11.5 %VO2max; exercise steps 42-73 %VO2max
  rest <- data.frame(VO2 = rep(300, 48), VCO2 = rep(240, 48),
                     RER = rep(0.8, 48), HR = rep(60, 48))
  vo2 <- seq(1100, 1900, length.out = 6)
  ex <- data.frame(VO2 = rep(vo2, each = 9),
                   VCO2 = rep(vo2 * seq(0.78, 0.92, length.out = 6), each = 9),
                   RER = rep(seq(0.78, 0.92, length.out = 6), each = 9),
                   HR = rep(seq(110, 160, length.out = 6), each = 9))
  for (inc in c(FALSE, TRUE)) {
    expect_no_warning(
      MFO(step_time = 20, db_MFO = ex, db_basal = rest, cv_var = "VO2",
          author = "Jeukendrup", VO2max = 2600, include_basal = inc))
  }
})


# --- Energy equivalents (SPEC 19, finding 1) ---
# Hand values. Frayn, VO2 = 1000, VCO2 = 850 ml/min:
#   CHO = 4.55*0.85 - 3.21 = 0.6575; FAT = 1.67*0.15 = 0.2505
#   classic: 0.6575*4 + 0.2505*9 = 4.8845
#   source (Peronnet & Massicotte 1991, Table 1, package decision for Frayn):
#            0.6575*3.8683 + 0.2505*9.7460 = 2.54340725 + 2.441373 = 4.98478025
# Jeukendrup (Table 5), CHO/FAT from the test above:
#   40 %: 0.75768*3.95 + 0.29898*9.75 = 2.992836 + 2.915055 = 5.907891
#   60 %: 1.48860*4.07 + 0.29538*9.75 = 6.058602 + 2.879955 = 8.938557
# Tolerance 1e-10: decimal arithmetic only.
test_that("energy_equivalents 'classic' and 'source' give the hand values", {
  f <- function(e) substrate_oxidation(1000, 850, "Frayn", energy_equivalents = e)$Kcal
  expect_equal(f("classic"), 4.8845, tolerance = 1e-10)
  expect_equal(f("source"), 4.98478025, tolerance = 1e-10)
  expect_equal(substrate_oxidation(1000, 850, "Frayn")$Kcal, 4.8845)  # default

  s_p <- substrate_oxidation(1000, 850, "Peronnet", energy_equivalents = "source")
  expect_equal(s_p$Kcal, s_p$CHO * 3.8683 + s_p$FAT * 9.7460, tolerance = 1e-12)

  s_j <- substrate_oxidation(VO2 = c(1200, 1800), VCO2 = c(1020, 1620),
                             author = "Jeukendrup", VO2max = 3000,
                             energy_equivalents = "source")
  expect_equal(s_j$Kcal, c(5.907891, 8.938557), tolerance = 1e-10)

  # Jeukendrup at rest: pure glucose pair -> Peronnet & Massicotte values
  s_r <- substrate_oxidation(300, 240, "Jeukendrup", phase = "rest",
                             energy_equivalents = "source")
  expect_equal(s_r$Kcal, s_r$CHO * 3.8683 + s_r$FAT * 9.7460, tolerance = 1e-12)

  expect_error(substrate_oxidation(1000, 850, "Frayn", energy_equivalents = "x"))
})

test_that("default energy_equivalents message is shown once per session", {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  run <- function(...) suppressWarnings(
    MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df,
        db_graded = VO2max_df, cv_var = "RER", author = "Frayn",
        vo2max_window = 40, ...))  # 40 s = 2 records: no rounding message
  rm(list = ls(.mfo_state), envir = .mfo_state)
  expect_message(r_c <- run(), "will change to \"source\".*in MFO 0.3.0")
  expect_no_message(run())
  rm(list = ls(.mfo_state), envir = .mfo_state)
  # Set explicitly: no message
  expect_no_message(r_s <- run(energy_equivalents = "source"))
  expect_no_message(run(energy_equivalents = "classic"))

  # MFO and Fatmax do not depend on energy equivalents; Kcal does
  expect_equal(r_s$MFO, r_c$MFO)
  expect_equal(r_s$FAT_MAX, r_c$FAT_MAX)
  # Truncated last step (FAT set to 0 after Kcal was computed) left out
  db <- r_s$MFO_db[r_s$MFO_db$FAT > 0, ]
  expect_equal(db$Kcal, db$CHO * 3.8683 + db$FAT * 9.7460, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(r_s$x_Kcal, r_c$x_Kcal)))
})


test_that("RER_cutoff accepts the named values of Amaro-Gahete et al. (2019)", {
  # Five 1-min steps with RER 0.88, 0.92, 0.94, 0.97, 1.02
  rer <- c(0.88, 0.92, 0.94, 0.97, 1.02)
  vo2 <- c(1000, 1200, 1400, 1600, 1800)
  db <- data.frame(VO2 = rep(vo2, each = 3), VCO2 = rep(vo2 * rer, each = 3),
                   RER = rep(rer, each = 3), HR = 100)
  n_kept <- function(cut) nrow(suppressMessages(
    calculate_vars(20, db, VO2max = 3000, author = "Frayn", step_duration = 60,
                   steady_state_duration = 60, RER_cutoff = cut)))
  # classic 1.00 -> 4, safe 0.95 -> 3, sedentary 0.93 -> 2, trained 0.90 -> 1
  expect_equal(sapply(c("classic", "safe", "sedentary", "trained"), n_kept),
               c(classic = 4, safe = 3, sedentary = 2, trained = 1))
  expect_equal(n_kept("safe"), n_kept(0.95))
  expect_equal(n_kept(NULL), 5)
  expect_message(calculate_vars(20, db, VO2max = 3000, author = "Frayn",
                                step_duration = 60, steady_state_duration = 60,
                                RER_cutoff = "safe"),
                 "RER cutoff \\(0.95\\): removed 2 step")
  expect_error(n_kept("strict"), "RER_cutoff must be")
  expect_error(n_kept(c(0.9, 1)), "RER_cutoff must be")
})


test_that("sporadic or empty HR in the xlsx never removes rows (SPEC 9.5)", {
  data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO",
       envir = environment())
  dir <- withr::local_tempdir()
  sporadic <- data.frame(MFO_df)
  sporadic$HR[c(5, 20, 61)] <- NA
  empty <- data.frame(basal_df)
  empty$HR <- NA
  openxlsx::write.xlsx(sporadic, file.path(dir, "MFO_df.xlsx"))
  openxlsx::write.xlsx(empty, file.path(dir, "basal_df.xlsx"))
  openxlsx::write.xlsx(data.frame(VO2max_df), file.path(dir, "VO2max_df.xlsx"))

  dbs <- read_MFO_databases(from = "folder", path = dir,
                            db_basal_name = "basal_df", db_MFO_name = "MFO_df",
                            db_graded_name = "VO2max_df")
  # v0.2.0 dropped 3 MFO rows (shifting every later step) and all basal rows
  expect_equal(nrow(dbs$participant_db_MFO), nrow(MFO_df))
  expect_equal(nrow(dbs$participant_db_basal), nrow(basal_df))
  expect_equal(sum(is.na(dbs$participant_db_MFO$HR)), 3)
  expect_equal(dbs$participant_db_MFO$VO2, MFO_df$VO2)
})
