# MFO_reliability() (SPEC 12). Expected values: a two-way ANOVA computed by
# hand, psych::ICC (ICC2 = ICC(A,1) with the McGraw & Wong CI), and a
# simulation with a known theoretical ICC.

.as_results <- function(Y, ids = NULL) {
  # participants x sessions matrix -> one row per test, ID = P<i>_S<j>
  n <- nrow(Y)
  k <- ncol(Y)
  data.frame(ID = paste0("P", rep(seq_len(n), k), "_S", rep(seq_len(k), each = n)),
             MFO_g_min = as.vector(Y))
}

test_that("ANOVA, ICC, SEM, MDC and agreement match a hand computation", {
  # Y (4 participants x 2 sessions): grand mean 3; row means 1.5, 2.5, 4, 4
  # -> MSR = 2 * 4.5 / 3 = 3; column means 2.5, 3.5 -> MSC = 4 * 0.5 / 1 = 2;
  # SST = 12 -> SSE = 12 - 9 - 2 = 1, MSE = 1/3.
  # ICC = (3 - 1/3) / (3 + 1/3 + 2 (2 - 1/3) / 4) = 0.64; SEM = sqrt(1/3).
  Y <- cbind(c(1, 2, 3, 4), c(2, 3, 5, 4))
  st <- .icc_a1(Y, 0.95)
  expect_equal(c(st$MSR, st$MSC, st$MSE), c(3, 2, 1 / 3))
  expect_equal(st$ICC, 0.64)
  rel <- suppressWarnings(MFO_reliability(.as_results(Y)))
  tb <- rel$table
  expect_equal(tb$SEM, sqrt(1 / 3))
  expect_equal(tb$MDC95, 1.96 * sqrt(2) * sqrt(1 / 3))
  expect_equal(tb$MDC90, 1.645 * sqrt(2) * sqrt(1 / 3))
  expect_equal(tb$CV, 100 * sqrt(1 / 3) / 3)
  expect_equal(tb$CV_log, 100 * (exp(.icc_a1(log(Y), 0.95)$SEM) - 1))
  # S2 - S1 = 1, 1, 2, 0: bias 1, SD sqrt(2/3), LoA 1 -/+ 1.96 SD
  pr <- rel$pairs
  expect_equal(pr$pair, "S2-S1")
  expect_equal(pr$bias, 1)
  expect_equal(pr$sd_diff, sqrt(2 / 3))
  expect_equal(c(pr$LoA_lower, pr$LoA_upper), 1 + c(-1.96, 1.96) * sqrt(2 / 3))
  half <- qt(0.975, 3) * sqrt(2 / 3) / 2
  expect_equal(c(pr$bias_lower, pr$bias_upper), 1 + c(-half, half))
  expect_null(rel$familiarization)
})

test_that("ICC and its CI equal psych::ICC (ICC2)", {
  skip_if_not_installed("psych")
  set.seed(4)
  for (k in 2:3) {
    Y <- matrix(rnorm(20 * k, 0.35, 0.1), 20, k) + rnorm(20, 0, 0.1)
    Y[, k] <- Y[, k] + 0.03
    p <- suppressMessages(psych::ICC(Y, lmer = FALSE))$results
    p <- p[p$type == "ICC2", ]
    st <- .icc_a1(Y, 0.95)
    expect_equal(c(st$ICC, st$lower, st$upper),
                 c(p$ICC, p$`lower bound`, p$`upper bound`), tolerance = 1e-10)
  }
})

test_that("on simulated tests the ICC is close to its theoretical value", {
  # MFO between participants: mean 0.34, SD 0.10 (variance 0.01); between
  # sessions a multiplicative factor with CV 0.21, so the within variance is
  # E[MFO^2] 0.21^2 = (0.01 + 0.34^2) 0.0441. Theoretical ICC = 0.01 /
  # (0.01 + 0.005539) = 0.644. With 2000 participants the SE is about 0.013.
  sim <- simulate_MFO_sessions(2000, 2, population = list(VO2max = c(3000, 0)),
                               n_steps = 2, seed = 31)
  res <- data.frame(ID = paste(sim$truth$id, sim$truth$session, sep = "_"),
                    MFO_g_min = sim$truth$MFO)
  # the between-session error is multiplicative: differences grow with the mean
  expect_warning(rel <- MFO_reliability(res), "Differences grow with the mean")
  theo <- 0.01 / (0.01 + (0.01 + 0.34^2) * 0.21^2)
  expect_equal(rel$table$ICC, theo, tolerance = 0.04 / theo)
  expect_true(rel$table$ICC_lower < rel$table$ICC & rel$table$ICC < rel$table$ICC_upper)
  # CV_log ~ the between-session CV of 21 %: 100 (exp(sd of log factor) - 1)
  expect_equal(rel$table$CV_log, 100 * (exp(sqrt(log(1 + 0.21^2))) - 1), tolerance = 0.1)
})

test_that("familiarization is flagged when session 1 is biased", {
  pop <- list(VO2max = c(3000, 0), MFO = c(0.40, 0.08), Fatmax = c(45, 8))
  sim <- simulate_MFO_sessions(40, 3, population = pop, cv_MFO = 0.05, cv_Fatmax = 0.05,
                               familiarization = c(MFO = -0.15, Fatmax = 0),
                               n_steps = 2, seed = 41)
  res <- data.frame(ID = paste(sim$truth$id, sim$truth$session, sep = "_"),
                    MFO_g_min = sim$truth$MFO, FAT_MAX_perVO2max = sim$truth$Fatmax)
  rel <- suppressWarnings(MFO_reliability(res))  # n = 40 > 30; multiplicative error
  fm <- rel$familiarization
  expect_equal(fm$variable, c("MFO_g_min", "FAT_MAX_perVO2max"))
  expect_true(fm$discard_session1[fm$variable == "MFO_g_min"])
  expect_false(fm$discard_session1[fm$variable == "FAT_MAX_perVO2max"])
  expect_equal(rel$pairs$pair, rep(c("S2-S1", "S3-S2"), 2))
  expect_output(print(rel), "Familiarization")
})

test_that("participants and sessions come from columns, a table or the ID", {
  Y <- cbind(c(1, 2, 3, 4), c(2, 3, 5, 4))
  res <- .as_results(Y)
  a <- suppressWarnings(MFO_reliability(res))
  tab <- data.frame(id = res$ID, participant = sub("_.*", "", res$ID),
                    session = sub(".*_", "", res$ID))
  b <- suppressWarnings(MFO_reliability(res, sessions = tab))
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(tab, f, row.names = FALSE)
  c3 <- suppressWarnings(MFO_reliability(res, sessions = f))
  d <- suppressWarnings(MFO_reliability(cbind(res, tab[, -1])))
  expect_equal(a$table, b$table)
  expect_equal(a$table, c3$table)
  expect_equal(a$table, d$table)
  # IDs with underscores inside the participant split at the last one
  ids <- .reliability_ids(data.frame(ID = c("lab_A_01_S1", "lab_A_01_S2")), NULL)
  expect_equal(ids$participant, c("lab_A_01", "lab_A_01"))
  # sessions ordered by their number
  expect_equal(.order_sessions(c("S10", "S2", "S1")), c("S1", "S2", "S10"))
  expect_equal(.order_sessions(c("pre", "post")), c("pre", "post"))
})

test_that("inputs and sample sizes are checked", {
  Y <- cbind(c(1, 2, 3, 4), c(2, 3, 5, 4))
  res <- .as_results(Y)
  expect_warning(MFO_reliability(res), "Koo & Li \\(2016\\) recommend at least 30")
  expect_error(MFO_reliability(res[res$ID %in% c("P1_S1", "P2_S1", "P1_S2", "P2_S2"), ]),
               "At least 3 participants")
  expect_error(MFO_reliability(res[grepl("_S1", res$ID), ]), "At least 2 sessions")
  expect_error(MFO_reliability(data.frame(ID = c("a", "b"), MFO_g_min = 1:2)), "<participant>_<session>")
  expect_error(MFO_reliability(data.frame(x = 1)), "ID column")
  expect_error(MFO_reliability(res, sessions = data.frame(id = "x")), "columns id")
  expect_error(MFO_reliability(res, sessions = data.frame(id = "x", participant = "a",
                                                          session = "b")), "IDs not in sessions")
  expect_error(suppressWarnings(MFO_reliability(res, variables = "nope")), "not in results")
  expect_error(suppressWarnings(MFO_reliability(res, conf = 2)), "conf")
  expect_error(MFO_reliability(list()), "data frame")
  # a participant missing in one session is left out with a warning
  res2 <- rbind(res, data.frame(ID = "P5_S1", MFO_g_min = 3))
  w <- capture_warnings(r <- MFO_reliability(res2))
  expect_match(w, "1 participant\\(s\\) without a value", all = FALSE)
  expect_equal(r$table$n, 4)
})

test_that("heteroscedasticity warns, CV_log is NA for non-positive values, plot works", {
  set.seed(5)
  m <- seq(0.1, 1, length.out = 30)
  Y <- cbind(m, m * (1 + c(-1, 1) * 0.3))
  res <- .as_results(Y)
  res$t <- res$MFO_g_min - 0.5
  w <- capture_warnings(rel <- MFO_reliability(res))
  expect_match(w, "Differences grow with the mean.*MFO_g_min", all = FALSE)
  expect_true(rel$pairs$hetero_r[rel$pairs$variable == "MFO_g_min"] > 0.9)
  expect_true(is.na(rel$table$CV_log[rel$table$variable == "t"]))
  p <- plot(rel)
  expect_s3_class(p, "ggplot")
  expect_error(plot(rel, pair = 2), "from 1 to 1")
})

test_that("Koo & Li classes, too few complete participants and no variables", {
  expect_equal(vapply(c(0.3, 0.6, 0.8, 0.95), .koo_li_class, ""),
               c("poor", "moderate", "good", "excellent"))
  expect_true(is.na(.koo_li_class(NA)))
  Y <- cbind(c(1, 2, 3, 4), c(2, 3, 5, 4))
  res <- .as_results(Y)
  # FatMax_zone_lower known in 2 participants only: not analysed, with a warning
  res$FatMax_zone_lower <- c(30, 31, NA, NA, 32, 33, NA, NA)
  w <- capture_warnings(r <- MFO_reliability(res))
  expect_match(w, "FatMax_zone_lower: fewer than 3 complete", all = FALSE)
  expect_equal(r$table$variable, "MFO_g_min")
  expect_error(suppressWarnings(MFO_reliability(res, variables = "FatMax_zone_lower")),
               "No variable could be analysed")
  expect_error(suppressWarnings(MFO_reliability(data.frame(ID = res$ID, x = 1))),
               "none of the default variables")
})
